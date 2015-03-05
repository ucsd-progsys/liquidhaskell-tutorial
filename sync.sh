#!/bin/bash

rsync --progress -r $1 $2@$3:$4
