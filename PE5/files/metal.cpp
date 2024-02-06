#include "kpop.h"
#include "metal.h"
#include "jazz.h"
#include "rock.h"
#include "music_band.h"


int MetalBand::play(MusicBand *other)
{
    double score;
    double C = 0;
    if(dynamic_cast<KPopBand*>(other)) C = 0.5;
    else if(dynamic_cast<MetalBand*>(other)) C = 1;
    else if(dynamic_cast<RockBand*>(other)) C = 1.5;
    else if(dynamic_cast<JazzBand*>(other)) C = 1.1;
    
    score = (get_fan_count() + 0.1* get_talent() *get_energy()) * C;
    set_energy(get_energy()*0.84);
    return score;
}

void MetalBand::rehearse(void) 
{   
    int energy = get_energy();
    set_energy(energy*0.92);
    set_talent(get_talent() - 5);
}