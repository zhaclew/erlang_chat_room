-module(client).
-define(PORT, 8000).

% API
% -compile(export_all).
-export([start/0, create_chanel/1, join_channel/1, exit_channel/1, talk/2, list_channel/0, user/1]).


start() ->
    register(?MODULE, spawn(fun() -> 
        {ok, Socket} = gen_tcp:connect("localhost", ?PORT, [binary, {packet, 0},{active, true}]),  
        ets:new(clientTab, [named_table, ordered_set]), 
        ets:insert(clientTab, {socket, Socket}), 
        handle(Socket) end)).

handle(Socket)->
    receive
        {tcp, Socket, Bin} ->
            case binary_to_term(Bin) of
                % 创建频道
                create_channel_ok ->
                    io:format("From Sever: create_channel_ok ~n"),
                    handle(Socket);
                % 列出所有频道
                {list_channel, Reply} ->
                    [showChannel(ChannelName) || {_, ChannelName} <- Reply],
                    handle(Socket);
                % 加入频道是否成功
                no_channel ->
                    io:format("add channel fail ~n"),
                    handle(Socket);
                join_success ->
                    io:format("add channel success ~n"),
                    handle(Socket);
                % 列出频道内所有的用户
                {user, UserList} ->
                    case UserList =/= [] of
                        false -> io:format("no_users_found ~n");
                        true -> [showUser(User) || {_, User} <- UserList]
                    end,
                    handle(Socket);
                % 频道内对话
                {data, _Socket, Reply} ->
                    io:format("data: ~p~n", [Reply]),
                    handle(Socket);
                % 退出频道
                exit_channel ->
                    io:format("client: exit_channel ok ~n"),
                    handle(Socket);
                _Any ->
                    io:format("client: error ~n"),
                    handle(Socket)
            end
    end.

% 创建频道
create_chanel(Name) ->
    [{socket, Socket}] = ets:tab2list(clientTab),
    gen_tcp:send(Socket, term_to_binary({create, Name})).

% 列出所有频道
list_channel() ->
    [{socket, Socket}] = ets:tab2list(clientTab),
    gen_tcp:send(Socket, term_to_binary(showList)).

% 加入频道
join_channel(Name) ->
    [{socket, Socket}] = ets:tab2list(clientTab),
    gen_tcp:send(Socket, term_to_binary({add, Name})).

% 退出频道
exit_channel(Name) ->
    [{socket, Socket}] = ets:tab2list(clientTab),
    % 向服务器发送退出的频道名
    gen_tcp:send(Socket, term_to_binary({exitChannel, Name})).

% 向频道讲话
talk(Name, Str) ->
    [{socket, Socket}] = ets:tab2list(clientTab),
    gen_tcp:send(Socket, term_to_binary({talk, Name, Str})).

% 列出频道所有用户
user(Name) ->
    [{socket, Socket}] = ets:tab2list(clientTab),
    gen_tcp:send(Socket, term_to_binary({showUsers, Name})).

% 打印用户
showUser(User) ->
    io:format("user : ~p~n", [User]).

% 打印频道
showChannel(ChannelName) ->
    io:format("channel : ~p~n", [ChannelName]).