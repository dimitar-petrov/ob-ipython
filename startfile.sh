# On recent Ubuntu, this may be necessary to use unbuffer command
export TCLLIBPATH=/usr/lib/tcltk/x86_64-linux-gnu
PS2=

function get_session_file()
{
    SESSION=`ls -t1 $(jupyter --runtime-dir)/kernel*.json | head -n1`
    echo $SESSION
}

function break_lines()
{
    sed 's/.\{160\}/&\
/g'
}

function escape_newlines()
{
    sed 's/\\n/\\\\n/g'
}

function pretty_json()
{
    while read json
    do
        echo "$json" |  python -m json.tool 2>/dev/null
    done
}

function summarize_base64()
{
    # Replace large BASE64-encoded images to a shorter example
    # to please emacs

    sed 's|[a-zA-Z0-9+/=]\{1000,\}|TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=|g'
}

function filter()
{
    OUT_FILE="$1"
    OUT_FILE=${OUTFILE:=/tmp/out.jsonlines}
    tee "$OUT_FILE" | escape_newlines | pretty_json | summarize_base64
}
