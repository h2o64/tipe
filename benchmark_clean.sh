#/bin/sh

for PATH in "plan1" "plan1b" "plan2" "plan2b" "plan3" "plan4"
do
	# Remove images
	rm out/$PATH/DB1_B/*
	rm out/$PATH/DB2_B/*
	rm out/$PATH/DB3_B/*
	rm out/$PATH/DB4_B/*
	# Wipe
	> out4/$PATH/benchmark-time.csv
	> out4/$PATH/benchmark-hough-pos.txt
	> out4/$PATH/benchmark-hough-rand.txt
done
