# MoveVoters
This tool determines if a scoring rule has a Non-Convergent Nash Equilibrium.

This is mostly likely to be interesting to you if you are interested in voting theory papers.

The easiest way to run this is use the [web app](http://www.dansted.org/movevoters.html). 

On Ubuntu 16.04, you can install this along with all required dependancies by entering

    apt-get install git-core
    git clone https://github.com/gmatht/MoveVoters.git
    cd MoveVoters
    ./install_dep.sh

You can then confirm that it works by running `./movevoters`. You should then get some sample output concluding with a confirmation that "Self diagnostic successful". You can then use the software offline at [http://localhost/movevoters.html](http://localhost/movevoters.html).
