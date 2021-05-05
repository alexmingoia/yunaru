#!/bin/bash
echo -e "\nRestoring database $PGDATABASE from S3 backups"

# Find the latest backup file
LATEST_FILE=$(aws s3 ls $S3_BUCKET | awk '{print $4}' | sort | tail -n 1)
echo -e "\nFound file $LATEST_FILE in bucket $S3_BUCKET"

# Restore from the latest backup file
echo -e "\nRestoring $PGDATABASE from $LATEST_FILE"
S3_TARGET=$S3_BUCKET/$LATEST_FILE
aws s3 cp $S3_TARGET - | pg_restore --dbname postgres --create --clean --no-owner
echo -e "\nRestore completed"
