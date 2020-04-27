function v
    set ARGS (count $argv)

    if test $ARGS = 0
        set DST (vifm --choose-dir - ".")
    else
        set DST (vifm --choose-dir - "@")
    end

    cd $DST
end
