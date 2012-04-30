#!/bin/sh

exec 2>> /tmp/stderr.log 1>> /tmp/stdout.log
export LANG=en_US.utf8
rm -rf /tmp/osoji*.json
cd $HOME/OsojiPhoto/
chmod 755 save_messages
echo "----------"
date
F=/tmp/osoji.`date '+%s'`.json
#おそうじフォト
wget -O $F --no-check-certificate "https://www.googleapis.com/plus/v1/activities?query=%23%E3%81%8A%E3%81%9D%E3%81%86%E3%81%98%E3%83%95%E3%82%A9%E3%83%88&language=ja&maxResults=20&orderBy=recent&pp=1&key=AIzaSyBJGmWGdbti3TCM4Ndynh3FopHSGRlFLnQ" && ./save_messages $F

#お掃除フォト
F=/tmp/osoji_kanji.`date '+%s'`.json
wget -O $F --no-check-certificate "https://www.googleapis.com/plus/v1/activities?query=%23%E3%81%8A%E6%8E%83%E9%99%A4%E3%83%95%E3%82%A9%E3%83%88&language=ja&maxResults=20&orderBy=recent&pp=1&key=AIzaSyBJGmWGdbti3TCM4Ndynh3FopHSGRlFLnQ" && ./save_messages $F

#かたづけフォト
F=/tmp/osoji_katazuke.`date '+%s'`.json
wget -O $F --no-check-certificate "https://www.googleapis.com/plus/v1/activities?query=%23%E3%81%8B%E3%81%9F%E3%81%A5%E3%81%91%E3%83%95%E3%82%A9%E3%83%88&language=ja&maxResults=20&orderBy=recent&pp=1&key=AIzaSyBJGmWGdbti3TCM4Ndynh3FopHSGRlFLnQ" && ./save_messages $F
