Fortran 95 JSON Parser
======================

Getting Started
---------------
    program example1

        ! Typical usage should only require an explicit use of the fson module.
        ! The other modules will be used privatley by fson as required.  
        use fson

        ! declare a pointer variable.  Always use a pointer with fson_value.
        type(fson_value), pointer :: value

        ! parse the json file
        value => fson_parse("test1.json")

        ! print the parsed data to the console
        call fson_print(value)    

        ! extract data from the parsed value        

        ! clean up
        call fson_destroy(value)

    end program example1

Example JSON
------------
This JSON will serve as a reference for the following examples.  
If you are not already familiar with JSON you can read more at:
<http://www.json.org/>
<http://en.wikipedia.org/wiki/JSON>

    {
        "name": {
            "first": "George",
            "last": "Crumb"
        },
        "age": 83,
        "website": "www.georgecrumb.net",
        "compositions": [
            {
                "title": "A Little Suite For Christmas, A.D. 1979",
                "instrument": "piano"
            }
        ]
    }

Extracting Data
---------------

Getting the data from the parsed fson_value to your variable is easy.  All extraction is performed through an overloaded call to fson_get.

    ! Root fson value.  Remember, always use a pointer with fson value.
    type(fson_value), pointer :: value

    ! The composer's age will be extract into this variable
    integer :: age

    ! Parse the file.  See the getting started example.
    
    ! Extract the age value.  Use the optional path parameter to specify nested values.
    call fson_get(value, "age", age)