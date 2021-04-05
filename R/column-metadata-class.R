
ColumnMetadata = R6::R6Class(classname = "ColumnMetadata",
  public = list(
    initialize = function(
      name, 
      definition = NA, 
      standard_name = standardize_name(name),
      format = NA,
      units = NA
    ) {
      private$.name = name
      private$.definition = definition
      private$.standard_name = standard_name
      private$.format = format
      private$.units = units
    },
    merge = function(other, priority = 'self') {
      if (self$name == other$name && priority == 'self') {
        if (is.na(self$definition)) {
          self$definition = other$definition
        }
        if (!is.na(other$standard_name) && other$standard_name != other$name) {
          self$standard_name = other$standard_name
        }
        if (is.na(self$format)) {
          self$format = other$format
        }
        if (is.na(self$units)) {
          self$units = other$units
        }
        return(self)
      } else if (self$name == other$name && priority == 'other') {
        if (!is.na(other$definition)) {
          self$definition = other$definition
        } 
        if (!is.na(other$standard_name) && other$standard_name != other$name) {
          self$standard_name = other$standard_name
        } 
        if (!is.na(other$format)) {
          self$format = other$format
        }
        if (!is.na(other$units)) {
          self$units = other$units
        }
        return(self)
      } else if (!is.na(self$standard_name) && (self$standard_name == other$name)) {
        self$standard_name = other$standard_name
        if (!is.na(other$definition)) {
          self$definition = other$definition
        }
        if (!is.na(other$format)) {
          self$format = other$format
        }
        if (!is.na(other$units)) {
          self$units = other$units
        }
        return(self)
      } else if (!is.na(other$standard_name) && (other$standard_name == self$name)) {
        self$name = other$name
        if (is.na(self$definition)) {
          self$definition = other$definition
        }
        if (is.na(self$format)) {
          self$format = other$format
        }
        if (is.na(self$units)) {
          self$units = other$units
        }
        return(self)
      } else if (!is.na(self$standard_name) && !is.na(other$standard_name) &&
        self$standard_name == other$standard_name &&
        (self$name == self$standard_name || other$name == other$standard_name)
      ) {
        if (self$name == self$standard_name) {
          self$name = other$name
        }
        return(self$merge(other, priority = 'self'))
      }
      rlang::abort(
        "Definitions can not be merged for 'self' and 'other'.", "ColumnMetadataError", 
        self = self, other = other)
    },
    as_text = function() {
      s = glue::glue("Name: {private$.name}\n", 
                     "Definition: {private$.definition}\n",
                     "Standard name: {private$.standard_name}\n",
                     "Format: {private$.format}\n",
                     "Units: {private$.units}\n")
      return(s)
    },
    as_list = function() list(name = private$.name, 
                              definition = private$.definition, 
                              standard_name = private$.standard_name,
                              format = private$.format,
                              units = private$.units)
  ),
  private = list(
    .name = character(),
    .definition = character(),
    .standard_name = character(),
    .format = character(),
    .units = character()
  ),
  active = list(
    name = function(x) {
      if (missing(x)) {
        return(private$.name)
      } else {
        private$.name = x
      }
    },
    definition = function(x) {
      if (missing(x)) {
        return(private$.definition)
      } else {
        private$.definition = x
      }
    },
    standard_name = function(x) {
      if (missing(x)) {
        return(private$.standard_name)
      } else {
        private$.standard_name = x
      }
    },
    format = function(x) {
      if (missing(x)) {
        return(private$.format)
      } else {
        private$.format = x
      }
    },
    units = function(x) {
      if (missing(x)) {
        return(private$.units)
      } else {
        private$.units = x
      }
    }
  )
)

