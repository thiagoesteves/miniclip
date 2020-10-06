# miniclip
This code is the result of the interview challenge from Miniclip

## Getting started ##
You need to clone the repository and download rebar/rebar3 (if it's not already available in your path).
```
git clone https://github.com/erlcloud/erlcloud.git
cd erlcloud
wget https://s3.amazonaws.com/rebar3/rebar3
chmod a+x rebar3
```
To compile and run miniclip server
```
make
```
PS: maybe you need to configure the number of file descriptors at your shell, the ideal is 4096 (ulimit -n 4096)

To run unit test and see the coverage
```
make test
```


### Security Credentials

you can provide them via `erlcloud` application environment variables at miniclip.hrl file
```erlang
%% AWS Credential definitions
-define(AWS_ACCESS_KEY_ID,     "XXXXXXXX").
-define(AWS_SECRET_ACCESS_KEY, "XXXXXXXX").
-define(AWS_REGION,            "XXXXXXXX").
```
### CloudFormation file

you can find the template for the AWS SQS and AWS DynamoDB in the folder aws

### Timesheet

you can find the timesheet with all major subtasks and duration in the folder doc