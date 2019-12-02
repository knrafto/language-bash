for x in true; do cat <<EOF; done
here doc
EOF

for x in true; do
    for y in true; do
        cat <<EOF
here doc
EOF
        true
    done
done

while true; do cat <<EOF; done
here doc
EOF

while true; do
    while true; do
        cat <<EOF
here doc
EOF
        true
    done
done

while true <<EOF
here doc
EOF
do
    true
done
