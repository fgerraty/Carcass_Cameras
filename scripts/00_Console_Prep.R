##########################################################################
# Carcass Cameras ########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 00: Console Prep ################################################
#-------------------------------------------------------------------------

# Load packages
packages <- c("tidyverse", "ggthemes", "overlap", "janitor", "glmmTMB", "mvabund")

pacman::p_load(packages, character.only = TRUE); rm(packages)

