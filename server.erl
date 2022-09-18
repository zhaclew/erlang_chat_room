-module(server).
-behaviour(gen_server).
-define(PORT, 8000).
-define(SERVER, ?MODULE).

% API
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2]).


start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    start_parallel_server(),
    ets:new(channel, [bag, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, initSuccess}.

start_parallel_server() ->
    {ok, Listen} = gen_tcp:listen(?PORT, [binary,{packet, 0},{active,true}]),
    spawn(fun() -> per_connect(Listen) end).

per_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> per_connect(Listen) end),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            case binary_to_term(Bin) of
                % 创建频道
                {create, Name} ->
                    Reply = create_chanel(Name),
                    gen_tcp:send(Socket, term_to_binary(Reply)),
                    loop(Socket);
                % 列出频道
                showList ->
                    list_all_channel(Socket),
                    loop(Socket);
                % 加入特定频道
                {add, Name}    ->
                    {ok, Reply} = add_channel(Name, Socket),
                    gen_tcp:send(Socket, term_to_binary(Reply)),
                    loop(Socket);
                % 退出当前频道
                {exitChannel, Name} ->
                    {ok, Reply} = ret_channel(Name, Socket),
                    gen_tcp:send(Socket, term_to_binary(Reply)),
                    loop(Socket);
                % 在频道内发送消息
                {talk, Name, Str} ->
                    channel_talk(Socket, Name, Str),
                    loop(Socket);
                % 列出频道内所有用户
                {showUsers, Name} ->
                    {ok, UserList} = find_user(Name),
                    gen_tcp:send(Socket, term_to_binary({user, UserList})),
                    loop(Socket);
                _Any ->
                    io:format("server error ~n"),
                    loop(Socket)
            end
    end.

% 创建频道
create_chanel(Name) ->
    ets:insert(channel, {create, Name}),
    create_channel_ok.

% 列出频道
list_all_channel(Socket) ->
    L = ets:lookup(channel, create),
    gen_tcp:send(Socket, term_to_binary({list_channel,L})).

% 加入特定频道
add_channel(Name, Socket) ->
    %判断频道是否存在
    List = [ChannelList || {create, ChannelList} <- ets:tab2list(channel)],  % 列出所有频道名
    io:format("~p~n", [List]),
    case lists:member(Name, List) of  % 查看所加入的频道名是否存在
        false  -> {ok, no_channel};
        true ->
            ets:insert(channel, {Name, Socket}),
            {ok, join_success}
    end.

% 退出频道
ret_channel(Name, Socket) ->
    % 删除表里频道名对应的套接字
    ets:delete_object(channel, {Name, Socket}),
    {ok, exit_channel}.

% 在频道内发送消息
channel_talk(Socket, Name, Str) ->
    % 查看该用户是否在频道内
    io:format("str = ~p~n", [Str]),
    ChannelName = ets:lookup(channel, Name),
    case lists:member({Name, Socket}, ChannelName) of
        true ->
            io:format("is channel ~n"),
            sendTalk(Name, Str);
        false -> false
    end.

% 列出频道内所有用户
find_user(Name) ->
    User = ets:lookup(channel, Name),
    io:format("UserList : ~p~n", [User]),
    {ok, User}.

% 将消息转发给所有频道内所有的套接字
sendTalk(Name, Str) ->
    % 将频道内所有的套接字找出来
    AllSocketName = ets:lookup(channel, Name),
    [sendStr(Socket, Str) || {_Name, Socket} <- AllSocketName].

sendStr(Socket, Str) ->
    gen_tcp:send(Socket, term_to_binary({data , Socket , Str})),
    io:format("send user : ~p~n", [Socket]).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
