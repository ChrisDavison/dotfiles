#! /usr/bin/env bash
svg_to_eps(){
    cairosvg "$1" -o "$1".ps
    ps2eps "$1".ps -f "$1".eps
    ps2pdf -dEPSCrop "$1".eps
}

eps_to_pdf(){
    ps2pdf -dEPSCrop "$1"
}
