defmodule Formulae.Compiler.AST do
  @moduledoc false

  # credo:disable-for-this-file Credo.Check.Refactor.CyclomaticComplexity
  case Application.compile_env(:formulae, :compiler) do
    :finitomata ->
      defmacro ast do
        Code.ensure_compiled!(Finitomata)

        quote generated: true, location: :keep do
          @fsm """
          idle --> |start!| active
          active --> |compile| active
          active --> |stop| stopped
          """

          use Finitomata, fsm: @fsm, auto_terminate: true

          @impl Finitomata
          @doc false
          def on_start(payload) do
            formulas = collect_existing()
            {:continue, struct(__MODULE__, payload: payload, formulas: formulas)}
          end

          @impl Finitomata
          @doc false
          def on_transition(:idle, :start!, _event_payload, %__MODULE__{} = state) do
            {:ok, :active, state}
          end

          def on_transition(
                :active,
                :compile,
                {formula, _options},
                %__MODULE__{formulas: formulas} = state
              )
              when is_map_key(formulas, formula) do
            {:ok, :active, state}
          end

          def on_transition(
                :active,
                :compile,
                {formula, options},
                %__MODULE__{formulas: formulas} = state
              )
              when is_binary(formula) do
            case Formulae.compile(formula, options) do
              nil ->
                {:ok, :active, state}

              compiled ->
                {:ok, :active, %{state | formulas: Map.put(formulas, formula, compiled)}}
            end
          end

          @spec formulas(id :: Finitomata.id(), name :: Finitomata.fsm_name()) :: %{
                  optional(binary()) => Formulae.t()
                }
          @doc """
          Returns all the compiled `Formulae` modules known to the system.
          """
          def formulas(id \\ nil, name \\ __MODULE__),
            do: Finitomata.state(id, name).payload.formulas

          @spec compile(
                  id :: Finitomata.id(),
                  name :: Finitomata.fsm_name(),
                  formula :: binary(),
                  options :: keyword()
                ) :: :ok
          @doc """
          Compiles a formula if it has not yet been compiled, otherwise it’s a noop.
          """
          def compile(id \\ nil, name \\ __MODULE__, formula, options)

          def compile(id, name, formula, options) when is_binary(formula),
            do: Finitomata.transition(id, name, {:compile, {formula, options}})

          @spec eval(
                  id :: Finitomata.id(),
                  name :: Finitomata.fsm_name(),
                  formula :: Formulae.t() | binary() | {binary(), keyword()},
                  binding :: keyword()
                ) ::
                  any()
          @doc """
          Evaluates a formula. If it has not been compiled yet,
          sinchronously compiles it first.
          """
          def eval(id \\ nil, name \\ __MODULE__, formulae, binding)

          def eval(_id, _name, %Formulae{} = f, binding), do: Formulae.eval(f, binding)

          def eval(id, name, formula, binding) when is_binary(formula),
            do: eval(id, name, {formula, []}, binding)

          def eval(id, name, {formula, options}, binding) when is_binary(formula),
            do: do_eval(id, name, {formula, options}, binding, true)

          defp do_eval(id, name, {formula, options}, binding, retry?) when is_binary(formula) do
            case formulas(id, name) do
              %{^formula => f} ->
                eval(id, name, f, binding)

              _ when retry? ->
                compile(id, name, formula, options)
                do_eval(id, name, {formula, options}, binding, false)

              _ ->
                nil
            end
          end
        end
      end

    _ ->
      defmacro ast do
        quote generated: true, location: :keep do
          use GenServer

          def start_link(opts \\ []) do
            {id, opts} = Keyword.pop(opts, :id)
            {name, opts} = Keyword.pop(opts, :name, __MODULE__)
            name = if is_nil(id), do: name, else: {:via, Registry, {id, name}}
            GenServer.start_link(__MODULE__, %__MODULE__{payload: opts}, name: name)
          end

          @impl GenServer
          def init(%__MODULE__{} = state) do
            formulas = collect_existing()
            {:ok, %{state | formulas: formulas}}
          end

          @spec formulas(id :: term(), name :: term()) :: %{optional(binary()) => Formulae.t()}
          @doc """
          Returns all the compiled `Formulae` modules known to the system.
          """
          def formulas(id \\ nil, name \\ __MODULE__)
          def formulas(nil, name), do: GenServer.call(name, :state).formulas

          def formulas(id, name),
            do: GenServer.call({:via, Registry, {id, name}}, :state).formulas

          @spec compile(id :: term(), name :: term(), formula :: binary(), options :: keyword()) ::
                  :ok
          @doc """
          Compiles a formula if it has not yet been compiled, otherwise it’s a noop.
          """
          def compile(id \\ nil, name \\ __MODULE__, formula, options)

          def compile(nil, name, formula, options) when is_binary(formula),
            do: GenServer.cast(name, {:compile, {formula, options}})

          def compile(id, name, formula, options) when is_binary(formula),
            do: GenServer.cast({:via, Registry, {id, name}}, {:compile, {formula, options}})

          @spec eval(
                  id :: Finitomata.id(),
                  name :: Finitomata.fsm_name(),
                  formula :: Formulae.t() | binary() | {binary(), keyword()},
                  binding :: keyword()
                ) ::
                  any()
          @doc """
          Evaluates a formula. If it hsa not been compiled yet,
          sinchronously compiles it first.
          """
          def eval(id \\ nil, name \\ __MODULE__, formula, binding)

          def eval(_id, _name, %Formulae{} = formula, binding),
            do: Formulae.eval(formula, binding)

          def eval(_id, _name, nil, _),
            do: nil

          def eval(id, name, formula, binding) when is_binary(formula),
            do: eval(id, name, {formula, []}, binding)

          def eval(nil, name, {formula, options}, binding),
            do: GenServer.call(name, {:eval, {{formula, options}, binding}})

          def eval(id, name, {formula, options}, binding),
            do:
              GenServer.call({:via, Registry, {id, name}}, {:eval, {{formula, options}, binding}})

          @impl GenServer
          @doc false
          def handle_call(:state, _from, %__MODULE__{} = state),
            do: {:reply, state, state}

          def handle_call(
                {:eval, {{formula, _options}, binding}},
                _from,
                %__MODULE__{formulas: formulas} = state
              )
              when is_map_key(formulas, formula),
              do: {:reply, eval(nil, nil, Map.get(formulas, formula), binding), state}

          def handle_call({:eval, {{formula, options}, binding}}, _from, %__MODULE__{} = state)
              when is_binary(formula) do
            {:noreply, %__MODULE__{formulas: %{^formula => f}} = new_state} =
              handle_cast({:compile, {formula, options}}, state)

            {:reply, eval(nil, nil, f, binding), new_state}
          end

          @impl GenServer
          @doc false
          def handle_cast(
                {:compile, {formula, _options}},
                %__MODULE__{formulas: formulas} = state
              )
              when is_map_key(formulas, formula),
              do: {:noreply, state}

          def handle_cast({:compile, {formula, options}}, %__MODULE__{formulas: formulas} = state)
              when is_binary(formula) do
            case safe_compile(formula, options) do
              :ok ->
                {:noreply, state}

              compiled ->
                {:noreply, %{state | formulas: Map.put(formulas, formula, compiled)}}
            end
          end

          defp safe_compile(formula, options) do
            Formulae.compile(formula, options)
          rescue
            error in [CompileError] ->
              Logger.error("Wrong syntax in formula: ‹" <> error.description <> "›")

            error in [Formulae.SyntaxError] ->
              Logger.error("Restricted call in formula: ‹" <> Exception.message(error) <> "›")

            error ->
              Logger.error(
                "Unknown formula error in formula: ‹" <> Exception.message(error) <> "›"
              )
          end
        end
      end
  end
end
