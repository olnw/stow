#!/bin/sh

# Configured sink and source to be used with Jack
SINK='alsa_output.pci-0000_27_00.6.analog-stereo'
SOURCE='alsa_input.pci-0000_27_00.6.analog-stereo'

# Suspend just the devices we want to use with Jack
pacmd suspend-sink $SINK true
pacmd suspend-sink $SOURCE true
