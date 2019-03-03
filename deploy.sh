#!/usr/bin/env bash

source ./env

OUT_FILE="deploy.yaml"

aws cloudformation package \
  --template-file=template.yaml \
  --s3-bucket "$FN_BUCKET" \
  --output-template-file "$OUT_FILE"

aws cloudformation deploy \
  --stack-name "ddns" \
  --region us-west-2 \
  --capabilities CAPABILITY_NAMED_IAM \
  --template-file "$OUT_FILE" \
  --parameter-overrides \
    SubDomain="$SUB_DOMAIN" \
    HostedZone="$HOSTED_ZONE" \
    HostedZoneId="$HOSTED_ZONE_ID" \
    InitialPublicIp="$INITIAL_PUBLIC_IP" \
    BucketName="$BUCKET_NAME"
