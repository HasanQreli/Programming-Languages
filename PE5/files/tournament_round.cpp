#include "tournament_round.h"

// TournamentRound functions goes here

TournamentRound::TournamentRound() { }
TournamentRound::TournamentRound(std::list<MusicBand*>_bands) { bands = _bands;}
TournamentRound::TournamentRound(std::vector<MusicBand*>_bands) { 
    for (const auto& band : _bands) {
        bands.push_back(band);
    }
}

std::size_t TournamentRound::size() { return bands.size(); }
    
/*TournamentRound::TournamentRound(TournamentRound& other) { 
    for (const auto& band : other.bands) {
        bands.push_back(band);
    }
}
TournamentRound::TournamentRound(TournamentRound&& other) { 
    bands = std::move(other.bands);
}*/

TournamentRound& TournamentRound::operator=(TournamentRound&& other) { 
    if(this != &other){
        bands.clear();
        for (const auto& band : other.bands) {
            bands.push_back(band);
        }
    }
    return *this; 
}
TournamentRound& TournamentRound::get_next_round() {
    std::list<MusicBand*> nextRound;
    int numPairs = bands.size() / 2;
    for (int i = 0; i < numPairs; ++i) {
        double score1 = bands.front()->play((bands.back()));
        double score2 = bands.back()->play((bands.front()));
        int oldfans1 = bands.front()->get_fan_count();
        int oldfans2 = bands.back()->get_fan_count();
        if (score1 >= score2) {
            if(score1 - score2 > oldfans2){
                bands.front()->set_fan_count(oldfans1 + oldfans2);
                bands.back()->set_fan_count(0);
            }
            else{
                bands.front()->set_fan_count(oldfans1 + score1 - score2);
                bands.back()->set_fan_count(oldfans2 +score2 -score1);
            }
            
            nextRound.push_back(bands.front());
        }
        else {
            if(score2 - score1 > oldfans1){
                bands.back()->set_fan_count(oldfans1 + oldfans2);
                bands.front()->set_fan_count(0);
            }
            else{
                bands.back()->set_fan_count(oldfans2 + score2 - score1);
                bands.front()->set_fan_count(oldfans1 +score1 -score2);
            }
            nextRound.push_back(bands.back());
        }
        bands.pop_front();bands.pop_back();
    }
    if(bands.size() != 0) nextRound.push_back(bands.front());
    
    bands = std::move(nextRound);
    return *this;
}

std::ostream& operator<< (std::ostream &os, TournamentRound &other) { 
    auto it = other.bands.begin();
    auto end = other.bands.end();
    end--;
    while(it!=end) {
        os << (*it)->get_name() << '\t';
        ++it;
    }
    if (!other.bands.empty()) {
        os << (*it)->get_name();
    }
    return os;
}