# On recent Ubuntu, this may be necessary to use the unbuffer command
export TCLLIBPATH=/usr/lib/tcltk/x86_64-linux-gnu

if [ -d "$LC_OBIPY_ROOT" ];
then
    echo env dir is $LC_OBIPY_ROOT
else
    echo env dir not defined
    exit 1
fi

CLIENT=$LC_OBIPY_ROOT/env/bin/client.py
if [ -f $CLIENT ] ;
then
    echo client script is $CLIENT
else
    echo "$CLIENT does not exist "
    exit 1
fi

if conda activate jcc-ai-gpu || conda activate jcc-ai-nogpus;
then
    conda info --env
else
    echo "conda not available"
    exit 1
fi


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

# Escape newlines for json.tool
function escape_newlines()
{
    sed 's/\\/\\\\/g'
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

function ipy_eval()
{
    echo "$@" | python $CLIENT  --conn-file `get_session_file` --execute | filter /tmp/eval_test.json
}
