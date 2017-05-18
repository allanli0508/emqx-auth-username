%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 EMQ Enterprise, Inc. (http://emqtt.io).
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_auth_username_cli).

-export([register_cli/0, unregister_cli/0]).

-include_lib("emqttd/include/emqttd_cli.hrl").

-export([run/1]).

-import(proplists, [get_value/2]).

-behaviour(clique_handler).

register_cli() ->
    F = fun() -> emqttd_mnesia:running_nodes() end,
    clique:register_node_finder(F),
    clique:register_usage(["users"], users_usage()),
    register_cmd().

run([]) ->
    io:format("--------------------------------------------------------------------------------~n"),
    io:format("users ~ts", [clique_usage:find(["users"])]),
    io:format("--------------------------------------------------------------------------------~n");

run(Cmd) ->
    clique:run(Cmd).

register_cmd() ->
    users_list(),
    users_add(),
    users_del().

unregister_cli() ->
    clique:unregister_usage(["users"]),
    unregister_cmd().

unregister_cmd() ->
    clique:unregister_command(["users", "list"]),
    clique:unregister_command(["users", "add"]),
    clique:unregister_command(["users", "del"]).

users_list() ->
    Cmd = ["users", "list"],
    Callback =
        fun (_, _, _) ->
            Usernames = emq_auth_username:all_users(),
            [clique_status:text(Usernames)]
        end,
    clique:register_command(Cmd, [], [], Callback).

users_add() ->
    Cmd = ["users", "add"],
    KeySpecs = [{'username', [{typecast, fun(Username) -> list_to_binary(Username) end}]},
                {'password', [{typecast, fun(Password) -> list_to_binary(Password) end}]}],
    Callback =
        fun (_, Params, _) ->
            Username = get_value('username', Params),
            Password = get_value('password', Params),
            Text = case {Username, Password} of
                {undefined, _} ->
                    users_usage();
                {_, undefined} ->
                    users_usage();
                {_, _} ->
                    case emq_auth_username:add_user(Username, Password) of
                    ok -> io_lib:format(" ~p add successfully~n", [Username]);
                    {error, already_existed} -> io_lib:format("Error:  ~p already existed~n", [Username]);
                    {error, Reason} -> io_lib:format("Error: ~p~n", [Reason])
                    end
                   end,
            [clique_status:text(Text)]
        end,
    clique:register_command(Cmd, KeySpecs, [], Callback).

users_del() ->
    Cmd = ["users", "del"],
    KeySpecs = [{'username', [{typecast, fun(Username) -> list_to_binary(Username) end}]}],
    FlagSpecs = [],
    Callback =
        fun (_, Params, _) ->
            Username = get_value('username', Params),
            Text = case {Username} of
                    {undefined} -> users_usage();
                    {_} ->
                        case emq_auth_username:remove_user(Username) of
                        ok -> io_lib:format(" ~p deleted successfully~n", [Username]);
                        {error, Reason} -> io_lib:format("Error: ~p~n", [Reason])
                        end
                   end,
            [clique_status:text(Text)]
        end,
    clique:register_command(Cmd, KeySpecs, FlagSpecs, Callback).

users_usage() ->
    ["\nusers list                                         List users\n",
     "users del username=<Username>                      Delete User\n",
     "users add username=<Username> password=<Password>  Add User\n"].
