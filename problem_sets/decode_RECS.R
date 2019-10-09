## Functions for decoding variables in the 2015 "RECS" data.
##
## Author: Ming-Chen Lu (mingchlu@umich.edu)
## Updated: October 8, 2019
#80: ---------------------------------------------------------------------------

# Functions to decode census division:------------------------------------------
decode_division_type = function(x) {
  if( !is.numeric(x) ) {
    stop('decode_division_type expects numeric input indexed from 1!')
  }
  
  switch(x,
         "New England",
         "Middle Atlantic",
         "East North Central",
         "West North Central",
         "South Atlantic",
         "East South Central",
         "West South Central",
         "Mountain North",
         "Mountain South",
         "Pacific"
  )
}

decode_all_division_types = function(x) {
  sapply(x, decode_division_type)
}

# Functions to decode main space heating fuel:----------------------------------
decode_fuelheat_type = function(x) {
  if( !is.numeric(x) ) {
    stop('decode_fuelheat_type expects numeric input indexed from 1!')
  }
  
  switch(x,
         "1" = "Natural gas from underground pipes",
         "2" = "Natural gas from underground pipes"
  )
}

decode_all_fuelheat_types = function(x) {
  sapply(x, decode_fuelheat_type)
}


