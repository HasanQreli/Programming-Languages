:- module(main, [is_vote_wasted/2, is_candidate_elected/2, candidate_count_from_city/3, all_parties/1, all_candidates_from_party/2, all_elected_from_party/2, election_rate/2, council_percentage/2, alternative_debate_setups/2]).
:- [kb].

% DO NOT CHANGE THE UPPER CONTENT, WRITE YOUR CODE AFTER THIS LINE

is_vote_wasted(City, PoliticalParty) :-
    not(elected(City, PoliticalParty, _)).


is_candidate_elected(Name, PoliticalParty) :-
    candidate(Name, PoliticalParty, City, Row),
    elected(City, PoliticalParty, ElectedRepresentativeCount),
    Row =< ElectedRepresentativeCount.

candidate_count_from_city([], City, 0).
candidate_count_from_city([Candidate|Rest], City, Count) :-
    not(candidate(Candidate, _, City, _)),
    candidate_count_from_city(Rest, City, Count1),
    Count is Count1.
candidate_count_from_city([Candidate|Rest], City, Count) :-
    candidate(Candidate, _, City, _),
    candidate_count_from_city(Rest, City, Count1),
    Count is Count1 + 1.





all_parties(ListOfPoliticalParties) :-
    findall(Party, party(Party, _), ListOfPoliticalParties).


all_candidates_from_party(PoliticalParty, ListOfCandidates) :-
    findall(Candidate, candidate(Candidate, PoliticalParty, _, _), ListOfCandidates).


all_elected_from_party(PoliticalParty, ListOfCandidates) :-
    findall(Candidate, is_candidate_elected(Candidate, PoliticalParty), ListOfCandidates).


election_rate(PoliticalParty, Percentage) :-
    all_candidates_from_party(PoliticalParty, AllCandidates),
    all_elected_from_party(PoliticalParty, ElectedCandidates),
    length(AllCandidates, Count1),
    length(ElectedCandidates, Count2),
    Percentage is (Count2 / Count1).


council_percentage(PoliticalParty, Percentage) :-
    to_elect(Count1),
    all_elected_from_party(PoliticalParty, ElectedCandidates),
    length(ElectedCandidates, Count2),
    Count1 > 0,
    Percentage is (Count2 / Count1).





alternative_debate_setups(DescriptionString, OrderedListOfCandidates) :-
    atom_chars(DescriptionString, Atomlist),
    helper1(Atomlist, OrderedListOfCandidates).

helper1([], []).
helper1([X|Y], [A|B]) :-
    party(Partyname, X),
    all_candidates_from_party(Partyname, Candidates1),
    member(A, Candidates1),
    helper1(Y, B),
    not(member(A, B)).






