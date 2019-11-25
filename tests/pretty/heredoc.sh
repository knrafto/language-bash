cat <<EOF
Hello world!
EOF

cat <<EOF1 <<EOF2
Here doc 1
EOF1
Here doc 2
EOF2

cat <<EOF > /dev/null
Hello world!
EOF
