# postprocess.R

# for stores with closure run lengths > 14, add a surge in Sales, locate the first
# sunday closure before the run length.  For the week leading up to that Sunday,
# multiply the forcasted sale on Monday x 3 = Mondaypeak.  Then fit a line from
# Mondaypeak to the sales fc-ed for that Friday, and interpolate Tues-Wed-Thurs Sales.

