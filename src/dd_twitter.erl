%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2014, Gert Meulyzer
%%% @doc
%%% A module to interface with Twitter.
%%% The code could use a cleanup.
%%% @end
%%% Created :  3 Jan 2014 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_twitter).

-compile(export_all).
-export([get_tweet/1, auth_header/0, get_usertimeline_tweet/1, reply_with_tweet/3, get_tweets/1, tweet_to_line/1, tweet/2, is_tweet/1]).
-export([periodic_get_mentions/0]).

-define(SingleTweetPattern, "https?://twitter.com/(\\w*)/status\\w?\\w?/(\\d*)").

get_with_auth(URL) ->
    {ok, {_,_, JSON}} = httpc:request(get, {URL, [auth_header()]}, [], []),
    JSON.

get_tweet(TweetID) when is_binary(TweetID) ->
    get_tweet(binary_to_list(TweetID));
get_tweet(TweetID) ->
    %% changed password and removed it from the source code. D'oh!.
    URL = "http://127.0.0.1:8080/1.1/statuses/show.json?id="++TweetID,
    JSON = get_with_auth(URL),
    get_usertimeline_tweet(mochijson:decode(JSON)).

auth_header() ->
    {ok, [User, Pass]} = application:get_env(dd, supertweet),
    auth_header(User, Pass).

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

-spec get_usertimeline_tweet({'struct', [any()]}) -> string().
get_usertimeline_tweet({struct, Twt}) ->
    {struct, User} = proplists:get_value("user", Twt),
    Nick = proplists:get_value("screen_name", User),
    Name = proplists:get_value("name", User),
    Text = proplists:get_value("text", Twt),
    Name ++ " ("++Nick++"): "++cleanup(Text).

store_id({struct, Twt}) ->
	Id = proplists:get_value("id", Twt),
	case application:get_env(dd, last_tweet) of
		undefined ->
			application:set_env(dd, last_tweet, Id);
		{ok, L} ->
			case Id > L of
				true -> 
					io:format("Storing ~p as last Tweet id~n", [Id]),
					application:set_env(dd, last_tweet, Id);
				false ->
					ok
			end
	end.

-spec reply_with_tweet(string(), pid(), [binary()]) -> ok.
reply_with_tweet(Tweet, Pid, Args) ->
    spawn(fun() ->
                  dd_connection:reply(Pid, Args, unicode:characters_to_binary(Tweet))
          end),
    ok.

get_mentions(ReplyPid, Args) ->
	%% https://api.twitter.com/1.1/statuses/mentions_timeline.json
	LastTweet = 
		case application:get_env(dd, last_tweet) of
			undefined -> 1;
			{ok, L} -> L
		  end,
	URL = io_lib:format("http://127.0.0.1:8080/1.1/statuses/mentions_timeline.json?count=1&since_id=~p",[LastTweet]),
	JSON = get_with_auth(URL),
	{array, Tweets} = mochijson:decode(JSON),
    [ spawn(fun() -> 
					reply_with_tweet(Tweet, ReplyPid, Args)
			end)
      || Tweet <- [ get_usertimeline_tweet(Twt) || Twt <- Tweets ]],
	[ store_id(Twt) || Twt <- Tweets ],
	ok.

-spec get_tweets(string()) -> [string()].
get_tweets(JSON) ->
    P = mochijson:decode(JSON),
    {struct, PropList} = P,
    {array, TwtLst} = proplists:get_value("results", PropList),
    [ tweet_to_line(T) || T <- TwtLst ].

-spec tweet_to_line({'struct', [any()]}) -> string().
tweet_to_line({struct, P}) ->
    Nick = proplists:get_value("from_user", P),
    Name = proplists:get_value("from_user_name", P),
    Text = proplists:get_value("text", P),
    Name ++ " ("++Nick++"): "++Text.

-spec handle_twitter_usertimeline(pid(), [binary()], binary()) -> ok.
handle_twitter_usertimeline(ReplyPid, Args, Username) ->
    URL = "http://127.0.0.1:8080/1.1/statuses/user_timeline.json?count=2&screen_name="++edoc_lib:escape_uri(binary_to_list(Username)),
    JSON = get_with_auth(URL),
    {array, TwtList} = mochijson:decode(JSON),
    [ spawn(fun() -> reply_with_tweet(Tweet, ReplyPid, Args) end)
      || Tweet <- [ get_usertimeline_tweet(Twt) || Twt <- TwtList ]],
    ok.

-spec is_tweet(URL :: binary()) -> {true, binary()} | false.
is_tweet(URL) ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    case re:run(URL, Regex, [{capture, all_but_first, binary}]) of
        {match, [_, TweetID]} ->
            {true, TweetID};
        _ -> false
    end.


-spec reply_if_single_tweet(binary()) -> ok | false.
reply_if_single_tweet(URL) ->
    case is_tweet(URL) of
        {true, TweetID} -> get_tweet(TweetID);
        false -> false
    end.


get_tweet_id_from_line(Line) ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    case re:run(Line, Regex, [{capture, all_but_first, binary}]) of
        {match, [_, TweetID]} ->
            TweetID;
        _ -> false
    end.

retweet_from_url(URL) ->
    ID = binary_to_list(get_tweet_id_from_line(URL)),
    retweetpost(ID),
    ok.

tst() ->
    URL = "http://127.0.0.1:8080/1.1/statuses/user_timeline.json?count=2&screen_name=G3rtm",
    JSON = get_with_auth(URL),
    io:format("Decoding"),
    {array, TwtList} = mochijson:decode(JSON),
    [ get_usertimeline_tweet(Twt) || Twt <- TwtList ].

strt() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets).

test_tweetpattern() ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    URL = "https://twitter.com/dibblego/statuses/420118599525617664",
    re:run(URL, Regex, [{capture, all_but_first, binary}]).

cleanup(Line) ->
    {ok, LF} = re:compile("\r", [caseless]),
    {ok, NL} = re:compile("\n", [caseless]),
    {ok, Gt} = re:compile("&gt;", [caseless]),
    {ok, Lt} = re:compile("&lt;", [caseless]),
    {ok, Amp} = re:compile("&amp;", [caseless]),
    {ok, Quot} = re:compile("&quot;", [caseless]),
    {ok, Apos} = re:compile("&apos;", [caseless]),
    lists:foldl(fun({Rgx,Repl}, Acc) -> 
                        re:replace(Acc, Rgx, Repl, [{return, list}, global]) 
                end, 
                Line, 
                [{Amp, "\\&"},
                 {Gt, ">"},
                 {Lt, "<"},
                 {Quot, "\""},
                 {Apos, "'"},
                 {LF, " "},
                 {NL, " "}
                 ]).


post_with_auth(Data) ->
    Method = post,
    URL = "http://127.0.0.1:8080/1.1/statuses/update.json",
    Header = [auth_header()],
    Type = "application/x-www-form-urlencoded",
    Body = url_encode(Data),
    io:format("Body: ~p~n",[Body]),
    HTTPOptions = [],
    Options = [],
    case httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options) of
        {ok, {_Status, _Headers, RBody}} -> 
            return_id(RBody);
        {ok, {_Status, RBody}} -> 
            return_id(RBody);
        Other -> 
            io:format("OTHER REPLY: ~p~n",[Other])
    end.

nick_allowed_to_tweet(Nickname) when is_binary(Nickname) ->
    nick_allowed_to_tweet(binary_to_list(Nickname));
nick_allowed_to_tweet(Nickname) ->
    {ok, WhiteList} = application:get_env(dd, tweeters),
    lists:member(Nickname, WhiteList).

return_id(RBody) ->
    {struct, Tweet} = mochijson:decode(RBody),
    proplists:get_value("id", Tweet).

retweetpost(ID) ->
    httpc:request(post, {"http://127.0.0.1:8080/1.1/statuses/retweet/"++ID++".json", [auth_header()], "application/x-www-form-urlencoded", []}, [], []).

tweet(Nickname, Text) when is_binary(Nickname) ->
    tweet(binary_to_list(Nickname), Text);
tweet(Nickname, Text) when is_binary(Text) ->
    tweet(Nickname, binary_to_list(Text));
tweet(Nickname, Text) ->
    case nick_allowed_to_tweet(Nickname) of
        true -> sendtweet(Nickname, Text);
        false -> disallowed
    end.

retweet(Nick, TweetID) when is_binary(TweetID) ->
    retweet(Nick, binary_to_list(TweetID));
retweet(Nick, ID) ->
    case nick_allowed_to_tweet(Nick) of
        true ->
            httpc:request(post, {"http://127.0.0.1:8080/1.1/statuses/retweet/"++ID++".json", [auth_header()], "application/x-www-form-urlencoded", []}, [], []);
        _ -> ok
    end.

sendtweet(Nickname, Text) ->
    Prefix = "<"++Nickname++"> ",
%%    Max140 = string:substr(Prefix++Text, 1, 140),
%%    Max140 = Prefix++Text,
    post_with_auth([{"status", Text},{"trim_user", "true"}]).

shorten_urls(Text) ->
    Parts = string:tokens(Text, " "),
    string:join([ shorten_if_url(Part) || Part <- Parts ], " ").

shorten_if_url(Text) ->
    case {string:substr(Text, 1, 7), string:substr(Text, 1, 8)} of
        {"http://", _} -> dd_url_handler:tinyurl(Text);
        {_, "https://"} -> dd_url_handler:tinyurl(Text);
        {_, _} -> Text
    end.

text_size(Text) ->
    Parts = string:tokens(Text, " "),
    lists:sum([ token_size(Part)+1 || Part <- Parts ]) - 1.

token_size(Text) ->
    case {string:substr(Text, 1, 7), string:substr(Text, 1, 8)} of
        {"http://", _} -> 23;
        {_, "https://"} -> 23;
        {_, _} -> length(Text)
    end.

url_encode(Data) ->
    url_encode(Data,"").

url_encode([],Acc) ->
    Acc;

url_encode([{Key,Value}|R],"") ->
    url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++
edoc_lib:escape_uri(Value));

url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++
edoc_lib:escape_uri(Value)).

periodic_get_mentions() ->
	receive
	after 1000*60*5 ->
			dd_twitter:get_mentions('Freenode',["#yfl"])
	end,
	periodic_get_mentions().
