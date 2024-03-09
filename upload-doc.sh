#!/bin/zsh
################################################################################
### This script uploads the documentation
### to a SFTP server specified via the following
### variables.
### Authorization uses a public key.
###
### 2023-04-20
################################################################################

dir=$PWD
# the path to the documentation directory
docdir="$dir"/doc/
# the path to the sftp key
sftpkey="~/.ssh/rp_doc_rsa"
sftpserver=rubenphilipp.com
sftpuser=ssh300000388
# the output path
sftpdir=/home/sites/site100007195/web/rubenphilipp/code/rp-clm


echo "Do you really want to upload the doc to the server?\n\n"
echo "The documentation from the directory:"
echo "$docdir\n"
echo "will then be uploaded to the directory:"
echo "$sftpdir\n"
echo "on the server:"
echo "$sftpserver"
echo "\n"
read -p "Press any key to confirm: "

sftp -i "$sftpkey" "$sftpuser"@"$sftpserver" <<EOF
cd $sftpdir
put -r $docdir
EOF

echo "Done."

################################################################################
### EOF upload-doc.sh
