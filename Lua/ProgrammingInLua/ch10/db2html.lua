function fwrite(fmt, ...)
  return io.write(string.format(fmt, ...))
end

function writeheader()
  io.write([[
    <html>
      <head>
        <title>Projects using Lua</title>
      </head>
      <body bgcolor="#FFFFFF">
        Here are brief descriptions of some projects around the
        world that use <a href="http://www.lua.org">Lua</a>.
        <br />
  ]])
end

function writetail()
  fwrite('</body></html>\n')
end

function entry1(o)
  count = count + 1
  local title = o.title or '(no title)'
  fwrite('<li><a href="#%d">%s</a></li>\n', count, title)
end

function entry2(o)
  count = count + 1
  fwrite('<hr />\n<h3>\n')
  
  local href = o.url and string.format(' href="%s"', o.url) or ''
  local title = o.title or o.org or 'org'
  fwrite('<a name="%d"%s>%s</a>', count, href, title)
  
  if o.title and o.org then
    fwrite('<br />\n<small><em>%s</em></small>', o.org)
  end
  fwrite('\n</h3>\n')
  
  if o.description then
    fwrite('%s<p>\n',
            string.gsub(o.description, '\n\n+', '<p>\n'))
  end
  
  if o.email then
    fwrite('Contact: <a href="mailto:%s">%s</a>\n',
            o.email, o.contact or o.email)
  elseif o.contact then
    fwrite('Contact: %s\n', o.contact)
  end
end

local inputfile = 'db.lua'
writeheader()

count = 0
f = loadfile(inputfile)

entry = entry1
fwrite('<ul>\n')
f()
fwrite('</ul>\n')

count = 0
entry = entry2
f()

writetail()
