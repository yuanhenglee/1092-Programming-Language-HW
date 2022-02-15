# define fillter
startsWithA = lambda x : x.startswith("A")

# test set
fruit = ["Apple", "Banana", "Pear", "Apricot", "Orange"]

# using filter
filter_object = filter(startsWithA, fruit)

print( "Original list:" )
print( fruit )

print( "Filtered list:" )
print( list( filter_object ) )