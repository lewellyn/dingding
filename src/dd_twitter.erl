%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2014, Gert Meulyzer
%%% @doc
%%% A module to interface with Twitter.
%%% The code could use a cleanup.
%%% @end
%%% Created :  3 Jan 2014 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_twitter).

-compile(export_all).
-export([get_tweet/1, get_usertimeline_tweet/1, reply_with_tweet/3, get_tweets/1, tweet_to_line/1, tweet/2, is_tweet/1]).
-export([periodic_get_mentions/0]).

-define(SingleTweetPattern, "https?://twitter.com/(\\w*)/status\\w?\\w?/(\\d*)").

get_tweet(TweetID) when is_binary(TweetID) ->
    get_tweet(binary_to_list(TweetID));
get_tweet(TweetID) ->
	JSON = oauth_twitter:get_tweet(TweetID),
    get_usertimeline_tweet(mochijson:decode(JSON)).

-spec get_usertimeline_tweet({'struct', [any()]}) -> string().
get_usertimeline_tweet({struct, Twt}) ->
    {struct, User} = proplists:get_value("user", Twt),
    Nick = proplists:get_value("screen_name", User),
    Name = proplists:get_value("name", User),
    Text = proplists:get_value("text", Twt),
    Name ++ " ("++Nick++"): "++ dd_helpers:cleanup_html_entities(Text).

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
	LastTweet = 
		case application:get_env(dd, last_tweet) of
			undefined -> 1;
			{ok, L} -> L
		  end,
	JSON = oauth_twitter:get_mentions(LastTweet),
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
	JSON = oauth_twitter:get_twitter_user_timeline(binary_to_list(Username)),
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

%% retweet_from_url(URL) ->
%%     ID = binary_to_list(get_tweet_id_from_line(URL)),
%%     retweetpost(ID),
%%     ok.

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

nick_allowed_to_tweet(Nickname) when is_binary(Nickname) ->
    nick_allowed_to_tweet(binary_to_list(Nickname));
nick_allowed_to_tweet(Nickname) ->
    {ok, WhiteList} = application:get_env(dd, tweeters),
    lists:member(Nickname, WhiteList).

return_id(RBody) ->
    {struct, Tweet} = mochijson:decode(RBody),
    proplists:get_value("id", Tweet).

tweet(Nickname, Text) when is_binary(Nickname) ->
    tweet(binary_to_list(Nickname), Text);
tweet(Nickname, Text) when is_binary(Text) ->
    tweet(Nickname, binary_to_list(Text));
tweet(Nickname, Text) ->
    case nick_allowed_to_tweet(Nickname) of
        true -> sendtweet(Nickname, Text);
        false -> disallowed
    end.

sendtweet(Text) ->
	sendtweet([], Text).

sendtweet(_, Text) ->
	return_id(oauth_twitter:send_tweet(Text)).

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
	dd_twitter:get_mentions('Freenode',["#yfl"]),
	receive
	after 1000*60*5 ->
			ok
	end,
	periodic_get_mentions().
