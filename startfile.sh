PS2=

function get_session_file()
{
    SESSION=`ls -t1 $(jupyter --runtime-dir)/kernel*.json | head -n1`
    echo $SESSION
}

