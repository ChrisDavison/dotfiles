# Description: desk for working with rumination/eating data

cd ~/engd/Sarascripts/Python

pyenv

# Classify extracted features using HMM parameters
classify () {
    python -m scripts.classify_features $1
}

# Train a Hidden Markov Model on extracted features
train () {
    python scripts/train.py $1
}

# Plot scatter of classified features
scatterplot () {
    python scripts/plot.py $1 --centroid
}

extract () {
    fn=$1
    t=$2
    fs=`infer_samplerate $fn`
    python -m scripts.extract_features $1 -s $fs -t $t
}

desk
