with Ada.Exceptions;
package MyExceptions is
    -- Pin exception
    PIN_Exception : exception;

    -- lock exception
    Lock_Exception : exception;

    -- stack exception
    Stack_Exception : exception;

    -- operator exception
    Operator_Exception : exception;

    -- calc exception
    Calc_Exception : exception;

    -- var exception
    Var_Exception : exception;

    -- syntex exception
    Syntex_Exception : exception;

end MyExceptions;