/* Remove files no longer needed */

call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs

zipname = 'pmos2.zip'
f = "ToKill"
list = ''
count = 0

DO WHILE LINES(f) > 0
    name = LineIn(f)
    IF name \= '' THEN DO
        list = list||' '||name
        count = count + 1
        Call DeleteAll name
        IF count > 9 THEN DO
            'zip -d 'zipname' 'list
            list = ''
            count = 0
        END /*IF*/
    END /*IF*/
END /*DO*/

IF count > 0 THEN DO
    'zip -d 'zipname' 'list
END /*IF*/
'copy 'zipname' I:'

return

/* Procedure to delete every file specified by a mask, including */
/* subdirectories (recursively) if appropriate.       */

DeleteAll: PROCEDURE
    parse arg mask
    call SysFileTree mask, list, 'OD'
    do i = 1 to list.0
        call DeleteAll list.i||'\*'
        call SysRmDir list.i
    end
    call SysFileTree mask, list, 'OF'
    do i = 1 to list.0
        call SysFileDelete list.i
    end
    return

