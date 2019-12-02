for x in true; do cat <<EOF; done
Here
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
Here
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
