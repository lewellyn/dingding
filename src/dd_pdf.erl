%% Copyright 2013 Gert Meulyzer

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
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% Module to get data from a pdf file (through pdfinfo)
%%% @end
%%% Created : 15 Jan 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_pdf).
-include("../include/dd_irc.hrl").
-compile(export_all).

%% ugh this is getting really ugly.
get_pdf_info(URL) when is_binary(URL) ->
    get_pdf_info(binary_to_list(URL));
get_pdf_info(URL) ->
    %%io:format("TheURL: ~s~n", [TheURL]),
    %%URL = binary_to_list(dd_url_handler:tinyurl(list_to_binary(TheURL))),
    %%io:format("URL: ~s~n", [URL]),
    %% create temporary file
    TempFile = hd(string:tokens(os:cmd("mktemp"), "\n")),
    %%io:format("TempFile: ~s~n", [TempFile]),
    %% download the file
    os:cmd("wget -q -O "++TempFile++" "++URL),
    %% run pdfinfo and capture output
    PdfInfo = string:tokens(os:cmd("pdfinfo "++TempFile), "\n"),
    %% delete the temporary file
    os:cmd("rm -f "++TempFile),
    %% parse output, retrieve 'title' field
    Props = [ {X, string:strip(Y)} || {X, Y} <-
                                          [ list_to_tuple(string:tokens(X, ":")) || X <- PdfInfo ]],
    io:format("Props: ~p~n", [Props]),
    Title = proplists:get_value("Title", Props, []),
    Pages = proplists:get_value("Pages", Props, []),
    %% return title.
    case {Title, Pages} of
        {[], []} -> <<>>; %% TODO: Clean this up. It's a dirty hack.
        {[], _} -> iolist_to_binary(["pdf: ", Pages, " pages."]);
        {_, []} -> iolist_to_binary(["pdf: ", Title]);
        {_, _} -> iolist_to_binary(["pdf: ", Title, ", ", Pages, " pages."])
    end.
