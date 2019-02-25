# aws-ddns

A small, simple, dyn dns solution backed by AWS services.

```
           Your Network  :  AWS
----------               :    -------------                 ----------                  ----------------
| server | [update ip] --:--> | s3 bucket | [put event] --> | lambda | [update r53] --> | r53 a record |
----------               :    -------------                 ----------                  ----------------
                         :
```
