blog = Прог
blog_id = 3940972473404160061
blog_url = http://draft.blogger.com/blogger.g?blogID=$(blog_id)
drafts_url = $(blog_url)\#draftposts

log = posting.log
opener = opener.sh

formatted = $(msg).format.html
html = $(msg).html

all: do

delete:
	google blogger delete --blog "$(blog)" --draft --title "$(title)"	

post:
	google blogger post --draft \
		--blog "$(blog)" --title "$(title)" --tag "$(tags)" $(html) \
		2>&1 | tee $(log)

create-opener:
	cat $(log) | sed 's/Post created: /open "/' | sed 's/$$/"/' >$(opener)

upload: post create-opener

preview:
	. ./$(opener)

local: format filter preview-local

preview-local:
	open $(html)

format:
	rst2html.py $(msg) >$(formatted)

filter:
	cat $(formatted) | blogspot-filter.py >$(html)

do: format filter delete upload preview

blog:
	open "$(blog_url)"

drafts:
	open "$(drafts_url)"

clean:
	-rm $(log) $(opener) $(html) $(formatted)
