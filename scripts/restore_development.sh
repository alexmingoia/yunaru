TIME=$(date "+%s")
BACKUP_FILE="$PGDATABASE.$TIME.pgdump"
ssh -i $WEBSERVER_KEY web@$WEBSERVER_HOST "source .env; pg_dump --format=custom > $BACKUP_FILE"
scp -i $WEBSERVER_KEY web@$WEBSERVER_HOST:/home/web/$BACKUP_FILE $BACKUP_FILE
ssh -i $WEBSERVER_KEY web@$WEBSERVER_HOST "rm $BACKUP_FILE"
pg_restore --dbname postgres --create --clean --no-owner $BACKUP_FILE
rm $BACKUP_FILE
