Pastebin
========

My first meaningful program on **Erland(OTP)/Cowboy/Erlydtl/Mnesia**

### Requirements
- [Rebar 2](https://github.com/rebar/rebar)
- [Erlang 17.4.1](https://www.erlang.org/downloads)

## Compilation

Reconfigure src/pastebin.app.src and run:

```shell script
$ rebar compile
```

## Usage

### Add data
```shell script
$ echo "My copy paste!" | curl -F 'file=<-' <your_hostname>
```

### Upload file

All uploads act as multipart forms by *POST*
```shell script
$ cat myfile.txt | curl -F 'file=<-' <your_hostname>
```

You can also add name to your paste
```shell script
$ echo "Nick; Age; Email;" | curl -F 'name=data.csv' -F 'file=<-' <your_hostname>
```

```shell script
$ cat myvid.wmv | curl -F 'name=coolvid.wmv' -F 'file=<-' <your_hostname>
```

Or just upload file with filename

```shell script
$ curl -F 'file=@data.csv' <your_hostname>
```

If file is already exist than you will get old link

#### Progress

To show progress add *-#/--progress-bar* option and/or pipe result to *more*

#### Standard progress
```shell script
$ curl -F 'file=@data.csv' <your_hostname> | more
```

#### Progress bar
```shell script
$ cat myfile | curl --progress-bar -F 'file=<-' <your_hostname> | more
```

### Change the link

To update associated file you must use binary *PUT* method

```shell script
$ curl --upload-file file <your_hostname>/7gxCXz | more
```

will update file with link **7gxCXz** with basic progress panel, if link not exists than error message will returns

### Delete link

To delete link from service you must use *DELETE* method.
```shell script
$ curl -X DELETE <your_hostname>/7gxCXz
```

will delete id=7gxCXz or error message if link not exist

### Get data</b>

To console output
```shell script
$ curl <your_hostname>/{XXXXX}
```

Output to disk
```shell script
$ curl <your_hostname>/{XXXXX} > mypaste.txt
```

To save data with content-disposition[filename] and content-type
```shell script
$ curl <your_hostname>/{XXXXX} -O -J
```

If 'name' wasn't set during upload then you have random file name

Also you can use browser to download any file ;)

### Client install
```shell script
$ curl <your_hostname>/client -O -J #Thats will download pastebin
$ chmod a+x pastebin
# mv pastebin /usr/local/bin # Optional
$ pastebin <file>
```

_Client can also download whole folders._

p.s. Fast and furious, not mem/cpu devourer even on Gb uploads/downloads))

25.05.2015
