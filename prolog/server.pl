:- encoding(utf8).
:- module(advisor_server, [
    server/1,
    test_analysis/0
]).

% Required HTTP libraries
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Load our knowledge base logic
:- use_module(knowledge_base).

%======================================================================
% Server Configuration & Routes
%======================================================================
% Start the server on a given port (e.g., 8000)
server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Starting Health Advisor Prolog Engine on port ~w~n', [Port]).

% Define the /advice API endpoint
:- http_handler(root(advice), handle_advice_request, []).

% CORS Configuration (Allow frontend to call this endpoint locally)
% Handled manually in the request handler.

%======================================================================
% Request Handlers
%======================================================================
handle_advice_request(Request) :-
    % Enable CORS headers and UTF-8 for the response
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Headers: Content-Type~n'),
    format('Content-Type: application/json; charset=utf-8~n'),
    
    % Handle potential CORS preflight OPTIONS request
    (   memberchk(method(options), Request)
    ->  format('~n'), % Just return the headers
        true
    ;   % Handle actual POST request
        memberchk(method(post), Request)
    ->  http_read_json_dict(Request, JSONIn),
        % format(user_error, 'Received Payload: ~p~n', [JSONIn]),
        
        % Delegate to knowledge base
        analyze_profile(JSONIn, ResultDict),
        
        % format(user_error, 'Sending Response: ~p~n', [ResultDict]),
        
        % Respond back to client
        reply_json_dict(ResultDict)
    ;   % Handle unsupported methods
        throw(http_reply(method_not_allowed('POST, OPTIONS')))
    ).

%======================================================================
% Local Testing Helper
%======================================================================
test_analysis :-
    TestProfile = _{
        age: 28,
        activityLevel: 'light',
        sleepHours: 5,
        stressLevel: 8
    },
    analyze_profile(TestProfile, Result),
    % pretty output
    writeln('--- Test Profile Analysis ---'),
    writeln(Result).
