# Client

The `update_ip` script in this directory should be installed on a system that
is generally always on. The script requires two parameters, `FILE` and `BUCKET`,
and assumes that AWS credentials are available in the environment.

## AWS Credentials

If you created the cloudformation stack in `../template.yaml` then you should
have an IAM user created called `{stack-name}-Writer` that has write-only
permissions to the configured bucket. You will need to [manually download
credentials for this user][aws-creds] and place them in `.aws/credentials`
or an alternate location.

An example credentials file might look like:

```
[default]
aws_access_key_id = foo
aws_secret_access_key = bar
aws_session_token = baz

[aws-dyndns]
aws_access_key_id = value from the creds file you downloaded
aws_secret_access_key = value from the creds file you downloaded
```

Note that unless you're using [STS] credentials you don't need to provide a
session token value.

It's also a good idea to set the default region for your dyndns user:

```
# file: ~/.aws/config

[default]
region = us-east-1

[aws-dyndns]
region = us-west-2
```

## Installing in Crontab

Crontab is a utility on many \*NIX variants that allow you to schedule tasks
on some regular interval. For more informaton on the syntax and structure
of crontab files consult `man crontab` on the target system.

Similar to `visudo`, `crontab` provides its own command for configuring tasks.
Depending on your target system you may need to provide either `VISUAL` or
`EDITOR` environment variables to get into your desired editor. As an example:

```bash
env VISUAL='vim' crontab -e
```

Once in your editor you can install the `update_ip` script like so:

```
* */2 * * * PATH="$PATH:/usr/local/bin:/absolute/path/to/update_ip/bin" AWS_DEFAULT_PROFILE="aws-dyndns" BUCKET="my-bucket" FILE="my-record-to-update" update_ip
```

`crontab` doesn't inherit your user path, which is why we need to augment the
PATH used with the locations of our `aws` and `update_ip` bin locations. We
also specify the profile we want the `aws` cli to use and which bucket/file
names we should use.

The above example will update the file in S3 every two hours.

### Note for macOS users

If you're installing this update script on a macOS system you see
`Permission denied` errors when installing your crontab file. This is typical
for newer installations (starting with Mojave, perhaps) and requires some
additional configuration.

First, figure out where your crontab lives:

```bash
$ which crontab
> /usr/bin/crontab
```

Then, go into **System Preferences > Security & Privacy > Privacy**. Once
there, scroll down the left-hand side of the screen until you see a **Full Disk
Access** option. Add an entry for crontab. If you're unsure how to navigate to
`/usr/bin` (as in the example above), you can hit **CMD+g** to open up the
path dialog and manually enter the `/usr/bin` path segment. Once there you can
select `crontab` from the list.

You should now be able to edit and install crontab files on your system.

[aws-creds]: https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html
[STS]: https://docs.aws.amazon.com/STS/latest/APIReference/Welcome.html
