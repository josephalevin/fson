Fortran 95 JSON Parser
======================

More examples coming soon...

Hello World
-----------
    program basic
        ! Typical usage should only require an explicit use of the fson module.
        ! The other modules will be used privatley by fson as required.  
        use fson

        ! declare a pointer variable.  Always use a pointer with fson_value.
        type(fson_value), pointer :: value

        ! parse the json file
        value => fson_parse("test1.json")

        ! print the parsed data to the console
        call fson_print(value)

        ! extract data out of the parse value
        ! code is working.  Will add examples soon


        ! clean up
        call fson_destroy(value)

    end program basic