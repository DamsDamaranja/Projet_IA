% ==========================
% Projet IA - Enquete policiere avec interface
% ==========================

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

% Charger les faits et regles de l'enquete
:- dynamic has_motive/2, was_near_crime_scene/2, has_fingerprint_on_weapon/2,
           has_bank_transaction/2, owns_fake_identity/2, eyewitness_identification/2.

% ----- Types de crimes -----
crime_type(assassinat).
crime_type(vol).
crime_type(escroquerie).

% ----- Suspects -----
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% ----- Faits -----
has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).

has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).

has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).

has_bank_transaction(bruno, escroquerie).
owns_fake_identity(sophie, escroquerie).

% ----- Regles -----
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    has_fingerprint_on_weapon(Suspect, vol).

is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    ( has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)
    ).

is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie);
    has_bank_transaction(Suspect, escroquerie);
    owns_fake_identity(Suspect, escroquerie).

% ----- Interface Web -----
:- http_handler(root(.), accueil, []).
:- http_handler(root(verdict), verdict, []).

% Lancer le serveur avec ?- serveur(Port).
serveur(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Page d'accueil avec formulaire
accueil(_Request) :-
    reply_html_page(
        title('Enquete policiere'),
        [ h1('Systeme Expert - Enquete'),
          form([action='/verdict', method='GET'], [
              p(['Nom du suspect : ', input([name=suspect, type=text])]),
              p(['Type de crime (vol, assassinat, escroquerie) : ',
                 input([name=crime, type=text])]),
              p(input([type=submit, value='Verifier']))
          ])
        ]).

% Traitement du verdict
verdict(Request) :-
    http_parameters(Request,
        [ suspect(SuspectAtom, []),
          crime(CrimeAtom, [])
        ]),
    atom_string(Suspect, SuspectAtom),
    atom_string(CrimeType, CrimeAtom),
    ( is_guilty(Suspect, CrimeType) ->
        Result = 'Coupable [OK]'
    ;   Result = 'Non coupable [KO]'
    ),
    reply_html_page(
        title('Resultat'),
        [ h2('Resultat de l enquete'),
          p(['Suspect : ', Suspect]),
          p(['Crime : ', CrimeType]),
          h3(['Verdict : ', Result]),
          a([href='/'], '<- Retour')
        ]).
