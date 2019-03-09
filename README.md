# aws-ddns

A small, simple, dynamic dns solution backed by AWS services.

#### Table of Contents

- [Motivation](#motivation)
- [Architecture Overview](#architecture-overview)
- [Usage](#usage)
  - [Account Infrastructure](#account-infrastructure)
  - [Initial Project Setup](#initial-project-setup)
  - [Ongoing Maintenance](#ongoing-maintenance)

## Motivation

This project is geared towards developers that have spare hardware lying around
at home, and potentially already work with AWS (or a competing cloud provider).

While many dynamic DNS providers already exist, some integrated neatly into
popular home routers, they all have various drawbacks. Most free tiers from
these providers require regular reconfirmation, lack custom domain support, or
support a restricted number of hosts. Most paid solutions range anywhere from 
$2 to $20 USD per month to use.

With very little hardware (an old computer, raspberry pi, whatever's on hand)
one can create a dynamic dns solution using AWS technologies that costs around
$1 USD per month to maintain.

## Architecture Overview

![architecture overview](./docs/img/aws-ddns-architecture.png)

`aws-ddns` is made up of a few components:

  - A computer on the private network that can upload to S3
  - An S3 bucket that contains ip address information
  - A Lambda function triggered by S3 create/update events
  - An `A` record pointed at the desired ip address

The script is responsible for uploading a file to S3 where the file name is the
DNS record to update and the contents of the file are the IP address to use for
that record. This allows you to update as many hosts within a single domain as
you need (more on multi-domain support further on). For more information
about the provided client script see the
[README in the client directory][client].

The S3 bucket's purpose is to store IP information for a given domain and to
trigger events when a file is created or updated. It is also a convenient
backup of the last known IP address for a given domain.

The Lambda function is responsible for listening to S3 events and updating the
associated Route53 ResourceRecordSet. It is configured to read the HostedZoneId
for the given domain from the environment.

The `A` record in Route53 is a ResourceRecordSet that points a domain name to
the public IP address of your private network.

## Usage

### Account Infrastructure

We make the following assumptions about your AWS account:

  - You have a means of [authenticating programmaticaly] with AWS
  (via User or [STS Assume Role])
  - You have permissions to create resources in your account
  - Your [domain name is managed by AWS] (i.e. with it's own HostedZone)
  - You have a separate S3 bucket to store your Lambda functions

If you don't already have an AWS account, [follow this guide].

If you don't already have an S3 bucket to host your Lambda functions you can
create one with this CloudFormation template:

```yaml
AWSTemplateFormatVersion: 2010-09-09
Description: Lambda Function storage.
Resources:
  LambdaFunctionBucket:
    Type: AWS::S3::Bucket
    -- Assumes you're re-building Lambda functions at least every 30 days.
    -- Remove this `Properties` section if this is not the case.
    Properties:
      LifecycleConfiguration:
        Rules:
          - ExpirationInDays: 30
            Status: Enabled
```

Record the S3 bucket name after creating this stack, we'll use it when deploying
new versions of the Lambda function.

### Initial Project Setup

A simple `deploy.sh` script is provided for most use cases to deploy `aws-ddns`.

In order for this deploy script to work we'll need to provide a file called
`env` in the root of the project defining the following variables:

```bash
# file: ./env

# The location of your lambda function code
FN_BUCKET="bucket-name-from-account-setup-section" 

# The sub-domain you want to create/update an A record for
SUB_DOMAIN="foo"

# The base domain you want to create your sub-domain in
HOSTED_ZONE="example.com"

# The _id_ of the hosted zone, not it's name. Needed for Route53 update calls.
HOSTED_ZONE_ID="the id of the hosted zone, _not_ it's name"

# The initial public IP you want to seed the template with.
INITIAL_PUBLIC_IP=$(curl -s https://ipinfo.io/ip)

# The desired bucket name to store your domain/ip-address combinations
BUCKET_NAME="desired-ip-bucket-name"
```

Now run the `./deploy.sh` script from the root of the directory. If you're using
STS credentials make sure they're still valid.

The deploy script will first deploy your lambda function to the desired S3
bucket then create a change set against your cloudformation stack. If at any
point this process fails you'll need to get the CloudFormation logs either
from the AWS CLI or Console.

A few minutes after the completion of your first deploy you should be able to
test that the `A` record was created successfully.

### Ongoing Maintenance

TODO

[client]: ./client/README.md
[authenticating programmaticaly]: https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html
[STS Assume Role]: https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-role.html
[domain name is managed by AWS]: https://aws.amazon.com/premiumsupport/knowledge-center/transfer-domain-to-aws/
[follow this guide]: https://aws.amazon.com/premiumsupport/knowledge-center/create-and-activate-aws-account/
