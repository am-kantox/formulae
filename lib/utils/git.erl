-module(git).

-export(['$handle_undefined_function'/2]).

'$handle_undefined_function'(Func, Args) ->
  GitCommand = atom_to_list(Func),
  GitArgs = [io_lib:format("~p ", [Arg]) || Arg <- Args],
  Command = ["git ", GitCommand, $\s, GitArgs],
  Output = os:cmd(Command),
  io:format("~s~n", [Output]).
