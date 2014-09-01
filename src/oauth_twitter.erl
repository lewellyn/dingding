%% Copyright (c) 2009-2011 Tim Fletcher <http://tfletcher.com/>

%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:

%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

%%
%% This is an example client for the Twitter API.
%%
%% Example usage:
%%
%%   $ make
%%   ...
%%   $ erl -pa ebin -pa path/to/erlang-oauth/ebin -s crypto -s ssl -s inets
%%   ...
%%   1> Consumer = {"...KEY...", "...SECRET...", hmac_sha1}.
%%   ...
%%   2> {ok, Client} = oauth_twitter:start(Consumer).
%%   ...
%%   3> {ok, Token} = oauth_twitter:get_request_token(Client).
%%   ...
%%   4> AuthorizeURL = oauth_twitter:authorize_url(Token).
%%   ...
%%   5> ok = oauth_twitter:get_access_token(Client, "...VERIFIER (PIN)...").
%%   ...
%%   6> {ok, Headers, JSON} = oauth_twitter:get_favorites(Client).
%%   ...
%%
%% Note that before fetching the access token (step 5) you need to have
%% authorized the request token and been given the a verifier PIN at twitter.
%%
-module(oauth_twitter).

-compile(export_all).

consumer() ->
	P = get_oauth_config(),
	ApiKey = proplists:get_value(api_key, P),
	ApiKeySecret = proplists:get_value(api_key_secret, P),
	{ApiKey, ApiKeySecret, hmac_sha1}.

start_apps() ->
	application:start(crypto),
	application:start(inets),
	application:start(asn1),
	application:start(public_key),
	application:start(ssl).

authenticate() ->
	Consumer = consumer(),
	{ok, Client} = start(Consumer),
	{ok, Token} = get_request_token(Client),
	io:format("~p~n",[authorize_url(Token)]),
	PinLine = io:get_line("PIN: "),
	Pin = string:sub_string(PinLine, 1, string:len(PinLine) - 1),
	io:format("PIN: ~s~n", [Pin]),
	AccessToken = get_access_token(Client, Pin),
	io:format("~p~n",[AccessToken]).

start(Consumer) ->
  oauth_client:start(Consumer).

get_request_token(Client) ->
  URL = "https://twitter.com/oauth/request_token",
  oauth_client:get_request_token(Client, URL).

authorize_url(Token) ->
  oauth:uri("https://twitter.com/oauth/authorize", [{"oauth_token", Token}]).

get_access_token(Client, Verifier) ->
  URL = "https://twitter.com/oauth/access_token",
  oauth_client:get_access_token(Client, URL, [{"oauth_verifier", Verifier}]).

get_oauth_config() ->
	{ok, P} = application:get_env(dd, oauth),
	ApiKey = proplists:get_value(api_key, P),
	ApiKeySecret = proplists:get_value(api_key_secret, P),
	AccessToken = proplists:get_value(access_token, P),
	AccessTokenSecret = proplists:get_value(access_token_secret, P),
	{ApiKey, ApiKeySecret, AccessToken, AccessTokenSecret}.

	
		

get_URL(URL, Params) ->
	{AK, AKS, AT, ATS}  = get_oauth_config(),
	oauth:get(URL, Params, {AK, AKS, hmac_sha1}, AT, ATS).

post_URL(URL, Params) ->
	{AK, AKS, AT, ATS}  = get_oauth_config(),
	oauth:post(URL, Params, {AK, AKS, hmac_sha1}, AT, ATS).

get_favs() ->
	get_URL("https://api.twitter.com/1.1/favorites/list.json", []).

get_mentions(Since_ID) ->
	get_URL("https://api.twitter.com/1.1/statuses/mentions_timeline.json", [{since_id, Since_ID}]).

get_twitter_user_timeline(Username) ->
	get_URL("https://api.twitter.com/1.1/statuses/user_timeline.json", [{count, 2}, {screen_name, Username}]).

get_tweet(TweetId) ->
	get_URL("https://api.twitter.com/1.1/statuses/show.json", [{id, TweetId}]).

send_tweet(Text) ->
	post_URL("https://api.twitter.com/1.1/statuses/update.json", [{status, Text}, {trim_user, "true"}]).

