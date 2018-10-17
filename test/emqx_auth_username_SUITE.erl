%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_username_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-define(TAB, emqx_auth_username).
-record(?TAB, {username, password}).

all() ->
    [{group, emqx_auth_username}].

groups() ->
    [{emqx_auth_username, [sequence],
      [emqx_auth_username_api, change_config, cli]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqx, emqx_auth_username]],
    Config.

end_per_suite(_Config) ->
    application:stop(emqx_auth_username),
    application:stop(emqx).

emqx_auth_username_api(_Config) ->
    ok = emqx_auth_username:add_user(<<"test_username">>, <<"password">>),
    User1 = #{username => <<"test_username">>},
    User2 = #{username => undefined, auth_method => <<"PLAIN">>, 
              auth_data => <<0,                                                             % U+0000
                             116,101,115,116,95,117,115,101,114,110,97,109,101,             % <<"test_username">>
                             0,                                                             % U+0000
                             112,97,115,115,119,111,114,100>>},                             % <<"password">>
    [{?TAB, <<"test_username">>, _HashedPass}] =
        emqx_auth_username:lookup_user(<<"test_username">>),
    ok = emqx_access_control:authenticate(User1, <<"password">>),
    {ok, _} = emqx_access_control:authenticate(User2, undefined),
    {ok, _} = emqx_access_control:authenticate(User2#{username  := <<"test_username">>,
                                                      auth_data := <<0,                                 % U+0000
                                                                    117,115,101,114,110,97,109,101,     % <<"username">>
                                                                    0,                                  % U+0000
                                                                    112,97,115,115,119,111,114,100>>}, undefined),
    {error, bad_authentication_method} = emqx_access_control:authenticate(User2#{auth_method  := <<"SCRAM-SHA-1">>}, undefined),
    {error, invalid_auth_data} = emqx_access_control:authenticate(#{auth_method => <<"PLAIN">>, auth_data => <<>>}, undefined),
    {error, invalid_auth_data} = emqx_access_control:authenticate(#{auth_method => <<"PLAIN">>}, undefined),
    ok = emqx_auth_username:remove_user(<<"test_username">>),
    {error, _} = emqx_access_control:authenticate(User1, <<"password">>),
    {error, _} = emqx_access_control:authenticate(User2, <<"password">>).

change_config(_Config) ->
    application:stop(emqx_auth_username),
    application:set_env(emqx_auth_username, userlist,
                        [{"id", "password"}, {"dev:devid", "passwd2"}]),
    application:start(emqx_auth_username),
    User1 = #{username => <<"id">>},
    User2 = #{username => <<"dev:devid">>},
    ok = emqx_access_control:authenticate(User1, <<"password">>),
    {error, password_error} = emqx_access_control:authenticate(User1, <<"password00">>),
    ok = emqx_access_control:authenticate(User2, <<"passwd2">>),
    %% clean data
    ok = emqx_auth_username:remove_user(<<"id">>),
    ok = emqx_auth_username:remove_user(<<"dev:devid">>).

cli(_Config) ->
    ok = emqx_auth_username:cli(["add", "username", "password"]),
    [{?TAB, <<"username">>, _M}] =
        emqx_auth_username:lookup_user(<<"username">>),
    ok = emqx_auth_username:cli(["del", "username"]),
    [] = emqx_auth_username:lookup_user(<<"username">>),

    ok = emqx_auth_username:cli(["add", "user1", "pass1"]),
    ok = emqx_auth_username:cli(["add", "user2", "pass2"]),
    UserList = emqx_auth_username:cli(["list"]),
    2 = length(UserList),
    emqx_auth_username:cli(usage).

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).
