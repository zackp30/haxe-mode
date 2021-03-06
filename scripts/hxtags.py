#!/usr/bin/env python
#
# Parses documentation XML generated by HaXe compiler when called with
# -xml option. You can use it in the following way:
# 
# $ hxtags.py --xml dodumentation.xml
#
# will generate TAGS from an existing documentation.xml
#
# $ hxtags.py --hxml build.hxml
#
# will try to compile build.hxml file, appending -xml option to it
# and generate TAGS file. You should not specify both option at once.

from optparse import OptionParser
from os import path
import os
import re
import sys
import socket
from subprocess import Popen
import StringIO
from xml.dom import minidom
from xml.dom.minidom import Document

connect_attempts = 10

parser = OptionParser()
parser.add_option(
    '-x', '--xml', dest = 'xml',
    help = '''The documentation XML file previously generated by HaXe compiler
-xml option.
Note that only the nodes with `file' attribute are processed, and only nodes 
with non-null `line' attribute are collected.''')

parser.add_option(
    '-b', '--hxml', dest = 'hxml',
    help = '''The HXML file to compile. This file is compiled by appending 
-xml option to it.
The resulting XML is later taken care of in the same way as with the
--xml option.''')

parser.add_option(
    '-f', '--file', dest = 'file',
    help = '''The HaXe source file, it will be compiled passing the 
--haxe-args arguments to HaXe compiler, appending -xml option.''')

parser.add_option(
    '-a', '--haxe-arguments', dest = 'arguments', default = '',
    help = '''The list of arguments to pass to HaXe compiler, ignored 
if --hxml or --xml options are present.''')

parser.add_option(
    '-c', '--haxe-compier', dest = 'compiler', default = 'haxe',
    help = '''The location of HaXe compiler, \'haxe\' by default.''')

parser.add_option(
    '-n', '--haxe-compier-host', dest = 'compiler_host', default = '127.0.0.1',
    help = '''When starting HaXe compilation server, the host to listen at.''')

parser.add_option(
    '-p', '--haxe-compier-port', dest = 'compiler_port', default = 1257,
    help = '''When starting HaXe compilation server, the port to listen at.''')

parser.add_option(
    '-t', '--tags-file', dest = 'tags_file', 
    help = '''The name of the TAGS file to output to, A special value `null\'
Specifies that the otput should be printed on stdout instead of a file.''')

(options, args) = parser.parse_args()

def parseFunctionDef(node, buf, function = 'f'):
    signature = node.getElementsByTagName(function)[0]
    overloads = {}
    names = signature.getAttribute('a').split(':')
    for arg in signature.childNodes:
        if arg.nodeType != 1:
            continue
        overload = None
        name = arg.nodeName
        try:
            overload = overloads[name]
        except:
            overload = []
            overloads[name] = overload
        sub = None
        if arg.hasChildNodes():
            sub = parseFunctionDef(
                signature, StringIO.StringIO(), arg.nodeName) 
            sub = '(' + sub.getvalue() + ')'
        else:
            sub = signature.getElementsByTagName(
                name)[len(overload)].getAttribute('path')
        overload.append(sub)
    atstart = True
    for value in overloads.values():
        if not atstart:
            buf.write(' | ')
        else:
            atstart = False
        if len(value) > 1:
            atarg = True
            counter = 0
            for arg in value:
                if not atarg:
                    buf.write('->')
                else:
                    atarg = False
                name = names[counter]
                if not name:
                    name = '_'
                buf.write(name)
                buf.write(':')
                if arg:
                    buf.write(arg)
                else:
                    buf.write('*')
        else:
            if names[0]:
                buf.write(names[0])
                buf.write(':')
            if value[0]:
                buf.write(value[0])
            else:
                buf.write('*')
    return buf

def parseDeclaration(node, buf):
    if node.getAttribute('public') == '1':
        buf.write('public ')
    if node.getAttribute('static') == '1':
        buf.write('static ')
    if node.getAttribute('get') == 'inline':
        buf.write('inline ')
    child = None
    for c in node.childNodes:
        if c.nodeType == 1:
            if c.getAttribute('a') is None:
                buf.write('var ')
            else:
                buf.write('function ')
            break
    params = node.getAttribute('params')
    if params:
        params = params.split(':')
        buf.write('<')
        first = True
        for p in params:
            if not first:
                buf.write(',')
            else:
                first = False
            buf.write(p)
        buf.write('>')
    buf.write(node.nodeName)

def findPosAtLine(file, line, word):
    pos = 0
    line_pos = 0
    line -= 1
    while pos < line:
        line_pos = file.find('\n', line_pos) + 1
        pos += 1
    return file.find(word, line_pos)

def findPosBeforeLine(file, line, first_word, second_word):
    pos = 0
    line_pos = 0
    line -= 1
    sub = None
    second_pos = -1
    while pos < line:
        # this assumes no one writes class and the name
        # of the class on different lines and doesn't put comments
        # in between.
        sub = file[line_pos : file.find('\n', line_pos)]
        
        first_pos = sub.find(first_word)
        if first_pos > -1:
            second_pos = sub.find(second_word, first_pos + len(first_word))
            if second_pos > -1:
                # lame attempt to check for comments
                # we could do some more advanced logistics here.
                # for instance, one good way to make it more certain
                # we found what we need is if we don't find
                # quotes prior to the class / enum name, or that
                # we find package declaration... but this seems to
                # work for many cases. Worst - we just give the position
                # of the class in the comment line or a string, and
                # whoever will see that, maybe will just move the coment :/
                if (sub.find('//', 0, first_pos) < 0 and
                    not sub[first_pos + len(first_word) : second_pos].strip()):
                    break
        line_pos += len(sub) + 1
        pos += 1
    return pos, line_pos + second_pos

def parseContainerDef(node, buf, line, file):
    entity_name = node.getAttribute('path')
    entity = node.nodeName
    pos, line_pos = findPosBeforeLine(file, line, entity, entity_name)
    params = node.getAttribute('params')
    buf.write(entity)
    buf.write(' ')
    buf.write(entity_name)
    if params:
        params = params.split(':')
        buf.write('<')
        first = True
        for p in params:
            if not first:
                buf.write(',')
            else:
                first = False
            buf.write(p)
        buf.write('>')
    for s in ['\x20\x7F', pos, ',', line_pos, '\n']:
        buf.write(s)

def parseHaXeDoc(xml, handler):
    xmldoc = minidom.parse(xml).documentElement
    
    # This is going to be huge :(
    file_cache = {}
    tags_chunks = {}
    for node in xmldoc.childNodes:
        if node.nodeType != 1:
            continue
        file = node.getAttribute('file')
        # We have the sources of this entity
        if file:
            entity_written = False
            for field in node.childNodes:
                if field.nodeType != 1:
                    continue
                line = field.getAttribute('line')
                # Unless we have line position, it is probably a file
                # from standard library. Ignore it, we don't want to
                # include the sources from standard library by default
                # especially so, there's no line position, so it's useless.
                if line:
                    if not file in file_cache:
                        with open(file, 'r') as hx_file:
                            file_cache[file] = hx_file.read()
                            sbuf = StringIO.StringIO()
                            for s in ['\x0C\x0A', file, ',',
                                      len(file_cache[file]), '\n']:
                                sbuf.write(s)
                            tags_chunks[file] = sbuf
                    buf = tags_chunks[file]
                    if not entity_written:
                        # This is a class / enum / typedef / interface
                        # none of these, unfortunately have position info
                        # so we'll have to figure that out on our own. We can
                        # search backwards from the line position of this line
                        # most chances are that the first time we hit
                        # 'class + ' ' + node.getAttribute('path') - that's
                        # our definition.
                        parseContainerDef(
                            node, buf, int(line), file_cache[file])
                        entity_written = True
                    # Here we need to append to the list the position of the
                    # field declaration and we can parse the type of the field
                    # which we can then append to the field name. It is also
                    # possible to find method access modifiers here.
                    parseDeclaration(field, buf)
                    firstName = None
                    for c in field.childNodes:
                        if c.nodeType == 1:
                            firstName = c.nodeName
                            break
                    if firstName:
                        buf.write('::')
                        parseFunctionDef(field, buf, firstName)
                    for s in ['\x20\x7F', line, ',',
                              findPosAtLine(
                                  file_cache[file], int(line),
                                  field.nodeName), '\n']:
                        buf.write(s)
    handler(tags_chunks)
# Because HaXe server has problems creating multiple connections
# we'll try to save the connection once we create it in any way.
free_socket = None

def writeTagsFile(tags_chunks):
    tags = open(options.tags_file or 'TAGS', 'w')
    for chunk in tags_chunks.values():
        tags.write(chunk.getvalue())
        chunk.close()
    tags.close()

def writeTagsStdout(tags_chunks):
    for chunk in tags_chunks.values():
        print chunk.getvalue()
        chunk.close()

def writeTagsSingleFile(tags_chunks):
    tags = open(options.tags_file or 'TAGS', 'w')
    try:
        tags.write(
            tags_chunks[unicode(
                path.abspath(options.file))].getvalue())
    except KeyError:
        # it could be just an empty file
        pass
    tags.close()

def writeTagsSingleStdout(tags_chunks):
    try:
        print tags_chunks[unicode(
            path.abspath(options.file))].getvalue()
    except KeyError:
        # it could be just an empty file
        pass

def tagsPrinter():
    result = None
    need_file = options.tags_file != 'null'
    if options.file:
        result = writeTagsSingleFile if need_file else writeTagsSingleStdout
    else:
        result = writeTagsFile if need_file else writeTagsStdout
    return result

def startHaXeServer():
    if not isHaXeServerUp(options.compiler_host, options.compiler_port):
        Popen([options.compiler, '--wait',
               options.compiler_host + ':' + str(options.compiler_port)])

def isHaXeServerUp(host = '127.0.0.1', port = 1257):
    global free_socket
    free_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    result = free_socket.connect_ex((host, port))
    return result == 0

def getFilePackage(file):
    handle = open(file)
    while True:
        line = handle.readline()
        if not line:
            break
        # this can be improved later to try to protect us against
        # block comments, but will do for now.
        line_comment = line.find('//')
        match = re.search('package(\s+(\w+\.)*(\w+))?\s*;', line)
        pos = match.start() if match else -1
        if match and line_comment < 0 or line_comment > pos:
            handle.close()
            return line[pos + 7 : match.end() - 1].replace('.', '/').strip(' \t')
    handle.close()
    raise Exception('''File does not appear to have package defined,
perhaps not a HaXe source''')

def compileFile():
    global free_socket
    startHaXeServer()
    if not free_socket:
        free_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        free_socket.connect((options.compiler_host, options.compiler_port))
    try:
        try:
            package = getFilePackage(options.file)
        except:
            # We couldn't find the package, but probably it's too early to
            # give up, so, let's try our chances, what if?
            package = ''
        main = options.file[options.file.rfind('/') + 1:]
        abspath = path.abspath(options.file)
        fqname = package + '.' + main if package else main
        cp = abspath[:-len(fqname)]
        free_socket.send(
            '-main %s\n-cp %s\n--no-output\n-xml ./doc.xml %s\n\0' % \
            (fqname, cp, '\n' + options.arguments if options.arguments else ''))
        response, errno = free_socket.recvfrom(4096)
        if response or errno:
            # There was something wrong in that how we built the command line
            # or the file can't be compiled with the current settings, rethrow
            # and exit, nothing we can do.
            sys.stderr.write(response)
            if free_socket: free_socket.close()
            sys.exit(-1)
    except socket.error, (value, message):
        print 'Couldn\' generate doc.xml because of \'%s\'' % message
        free_socket.close()
        # Sometimes the server would not respond, or we'd get a broken pipe
        # or disconnect etc. It's not very stable... so just try it again.
        compileFile()
    else:
        parseHaXeDoc('./doc.xml', tagsPrinter())

def compileHxml():
    global free_socket, connect_attempts
    startHaXeServer()
    if not free_socket:
        free_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        free_socket.connect((options.compiler_host, options.compiler_port))
    try:
        free_socket.send(
            '%s\n--no-output\n-xml ./doc.xml %s\n\0' % \
            (options.hxml,
             '\n' + options.arguments if options.arguments else ''))
        print 'sent to haxe: <%s\n--no-output\n-xml ./doc.xml %s>' % \
            (options.hxml,
             '\n' + options.arguments if options.arguments else '')
        response, errno = free_socket.recvfrom(4096)
        print 'haxe responded: %s' % response
        if response or errno:
            # There was something wrong in that how we built the command line
            # or the file can't be compiled with the current settings, rethrow
            # and exit, nothing we can do.
            sys.stderr.write(response)
            if free_socket: free_socket.close()
            sys.exit(-1)
    except socket.error, (value, message):
        print 'Couldn\' generate doc.xml because of \'%s\'' % message
        free_socket.close()
        # Sometimes the server would not respond, or we'd get a broken pipe
        # or disconnect etc. It's not very stable... so just try it again.
        if reconnect_attemps > 0:
            reconnect_attemps -= 1
            compileHxml()
        else:
            try:
                Popen([options.compiler, '--no-output', '-xml', './doc.xml',
                       options.hxml, options.arguments if options.arguments else '')])
            except Exception as e:
                sys.stderr.write(str(e))
                if free_socket: free_socket.close()
                sys.exit(-1)
    else:
        parseHaXeDoc('./doc.xml', tagsPrinter())
                    
if options.xml:
    parseHaXeDoc(options.xml, tagsPrinter())
elif options.file:
    compileFile()
elif options.hxml:
    compileHxml()

if free_socket: free_socket.close()
