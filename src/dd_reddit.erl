%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2014, Gert Meulyzer
%%% @doc
%%% Module for the reddit api
%%% @end
%%% Created :  5 Jan 2014 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_reddit).
-compile(export_all).

%% TODO: http://newexception.com/auto-submit-your-article-link-with-the-reddit-api

strt() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets).
%% inets:start(

auth_header() ->
    {ok, {User, Pass}} = application:get_env(dd, reddit),
    auth_header(User, Pass).

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

get_with_auth(URL) ->
    {ok, {_,_, JSON}} = httpc:request(get, {URL, [auth_header()]}, [], []),
    JSON.

-spec post_with_auth(URL, Data) -> string() when
      URL :: string(),
      Data :: [{string(), string()}].
post_with_auth(_URL, Data) ->
    Method = post,
    URL = "https://foauth.org/oauth.reddit.com/api/submit",
    Header = [auth_header()],
    Type = "application/json",
    Body = mochijson:encode({struct, Data}),
    io:format("Body: ~p~n",[Body]),
    HTTPOptions = [],
    Options = [],
    R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
    {ok, {{_, _ReturnCode, _State}, _Head, Body}} = R.

submit_link(URL) when is_binary(URL) ->
    submit_link(binary_to_list(URL));
submit_link(URL) ->
    API = "https://foauth.org/foauth.reddit.com/api/submit?sr=test&kind=link&url="++URL,
    get_with_auth(API).

%% api_type the string json
%% captcha	the user's response to the CAPTCHA challenge
%% extension extension used for redirects
%% iden	the identifier of the CAPTCHA challenge
%% kind	one of (link, self)
%% resubmit boolean value
%% save	boolean value
%% sendreplies	boolean value
%% sr	name of a subreddit
%% text raw markdown text
%% then one of (tb, comments)
%% title title of the submission. up to 300 characters long
%% uh / X-Modhash header	a modhash
%% url	a valid URL

tst1() ->
    get_with_auth("https://foauth.org/oauth.reddit.com/api/v1/me").

tst() ->
    post_with_auth("https://foauth.org/oauth.reddit.com/api/submit",
                   [{api_type, "json"},
                    {kind, "link"},
                    {save, "true"},
                    {sr, "test"},
                    {text, "A test link to see if this API call works"},
                    {title, "Testing my program"},
                    {r, "test"},
                    {url, "http://www.reddit.com/dev/api"}]).
