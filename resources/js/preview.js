// Copyright (c) 2007 Chris Purcell.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

function $(element) {
  if (document.getElementById)
    return document.getElementById(element);
  else if (document.all)
    return document.all[element];
  else
    return null;
}

String.prototype.replaceEvalGl = function(regex, fn) {
  var head = "";
  var tail = "" + this;
  while (m = tail.match(regex)) {
    head += tail.substring(0,m.index) + fn(m);
    tail = tail.substring(m.index + m[0].length);
  }
  return head + tail;
}

//// The markup rules ////////////////////////////////////////////////////////
MarkupRule = function(regex, rule) {
  this.regex    = regex;
  this.rule     = rule;
  this.children = [ ];
}
MarkupRule.prototype.clone = function() {
  var objectClone = new this.constructor();
  for (var property in this)
    objectClone[property] = this[property];
  return objectClone;
}
MarkupRule.prototype.setChildren = function(children) {
  this.children = children;
}
ElementRule = function(params) {
  return new MarkupRule(params["regex"], function (r) {
    var text = "";
    if ("capture" in params)
      text = r[params["capture"]];
    if (text) {
      if ("replaceRegex" in params)
        text = text.replace(params["replaceRegex"], params["replaceString"]);
      var tag = "<" + params["tag"] + ">";
      var endtag = "</" + params["tag"] + ">";
      if (!("tag" in params))
        tag = endtag = "";
      return tag + this.markUp(text) + endtag;
    } else if ("tag" in params)
      return "<" + params["tag"] + " />";
    else
      return "";
  });
}

function toXHTML(wikiText) {
  wikiText = wikiText.replace(/&/g, "&amp;");
  wikiText = wikiText.replace(/</g, "&lt;");
  wikiText = wikiText.replace(/>/g, "&gt;");
  wikiText = wikiText.replace(/"/g, "&quot;");
  return toXHTML.root.markUp(wikiText);
}

  // A header is text within equals signs (=)
toXHTML.h1 = new ElementRule({ tag: "h1", capture: 2,
  regex: /(^|\n)[ \t]*={1}[ \t](.+?)[ \t]*=*\s*(\n|$)/ });
toXHTML.h2 = new ElementRule({ tag: "h2", capture: 2,
  regex: /(^|\n)[ \t]*={2}[ \t](.+?)[ \t]*=*\s*(\n|$)/ });
toXHTML.h3 = new ElementRule({ tag: "h3", capture: 2,
  regex: /(^|\n)[ \t]*={3}[ \t](.+?)[ \t]*=*\s*(\n|$)/ });
toXHTML.h4 = new ElementRule({ tag: "h4", capture: 2,
  regex: /(^|\n)[ \t]*={4}[ \t](.+?)[ \t]*=*\s*(\n|$)/ });
toXHTML.h5 = new ElementRule({ tag: "h5", capture: 2,
  regex: /(^|\n)[ \t]*={5}[ \t](.+?)[ \t]*=*\s*(\n|$)/ });
toXHTML.h6 = new ElementRule({ tag: "h6", capture: 2,
  regex: /(^|\n)[ \t]*={6}[ \t](.+?)[ \t]*=*\s*(\n|$)/ });

  // hr is a line of 4 dashes (-)
toXHTML.hr = new ElementRule({ tag: "hr", regex: /(^|\n)\s*----\s*(\n|$)/ });

  // br is two backslashes (\)
toXHTML.br = new ElementRule({ tag: "br", regex: /\\\\/ });

  // Preformatted blocks are wrapped in {{{...}}}
toXHTML.preBlock = new ElementRule({ tag: "pre", capture: 2,
  regex: /(^|\n){{{\n?(.*?(\n.*?)*?)}}}(\n|$)/ });

  // tt inlines are also wrapped in {{{...}}}
toXHTML.tt = new ElementRule({ tag: "tt",
  regex: /{{{(.*?(?:\n.*?)*?)}}}/, capture: 1 });

  // Unordered and ordered lists start with * or #
toXHTML.ulist = new ElementRule({ tag: "ul",
  regex: /(^|\n)(\*[^*#].*(\n|$)([*#]{2}.*(\n|$))*)+/, capture: 0,
  replaceRegex: /(^|\n)[*#]/g, replaceString: "$1" });
toXHTML.olist = new ElementRule({ tag: "ol",
  regex: /(^|\n)(#[^*#].*(\n|$)([*#]{2}.*(\n|$))*)+/, capture: 0,
  replaceRegex: /(^|\n)[*#]/g, replaceString: "$1" });
toXHTML.li    = new ElementRule({tag:"li",regex:/.+(\n[*#].+)*/,capture:0});

  // Tables
toXHTML.table = new ElementRule({ tag: "table",
  regex: /(^|\n)(\|.*\|[ \t]*(\n|$))+/, capture: 0 });
toXHTML.tr    = new ElementRule({ tag: "tr",
  regex: /(^|\n)(\|.*)\|[ \t]*(\n|$)/, capture: 2 });
toXHTML.td    = new ElementRule({ tag: "td",
  regex: /[|]+([^|]*)/, capture: 1 });

  // Kinds of text block:
  //   - paragraph is the fallback for the root rule
  //     and consists of blocks of text separated by blank lines
  //   - singleLine is used within lists
toXHTML.singleLine = new ElementRule({ regex: /.+/, capture: 0 });
toXHTML.paragraph  = new ElementRule({ tag: "p",
  regex: /(^|\n)([ \t]*[^\s].*(\n|$))+/, capture: 0 });

  // Strongly emphasised text is surrounded by double-* characters
toXHTML.strong   = new ElementRule({ tag: "strong", capture: 1,
  regex:/\*\*([^*]*(?:\*[^*]+)*)\*\*/ });

  // Emphasised text is surrounded by double-/ characters
  // It must skip http:// or ftp:// internally
  // (This would be a lot easier to write with negative lookbehind!)
toXHTML.em       = new ElementRule({ tag: "em", capture: 1,
  regex:"\\/\\/(" + // Starts with a double-/
          "[^\\/hf]*(?:" +
            "\\/?(?:http:\\/?|ftp:\\/?)*(?:" +
              "h(?:t(?:tp?)?)?" + "|" +
              "f(?:tp?)?" + "|" +
              "(?:" +
                "h[^t\\/hf]" + "|" +
                "ht[^t\\/hf]" + "|" +
                "htt[^p\\/hf]" + "|" +
                "http[^:\\/hf]" + "|" +
                "http:[^\\/hf]" + "|" +
                "http:\\/[^\\/hf]" + "|" +
                "http:\\/\\/" + "|" +
                "f[^t\\/hf]" + "|" +
                "ft[^p\\/hf]" + "|" +
                "ftp[^:\\/hf]" + "|" +
                "ftp:[^\\/hf]" + "|" +
                "ftp:\\/[^\\/hf]" + "|" +
                "ftp:\\/\\/" +
              ")" +
              "[^\\/hf]*" +
            ")" + "|" +
            "\\/[^\\/hf][^\\/hf]*" +
          ")*" +
        ")" +
        "\\/\\/" // Ends with a double-/
});

  // Links
toXHTML.linkPattern  = "[^\\]|\\n]*(?:\\][^\\]|\\n]+)*";
toXHTML.urlProtocols = "(?:http|https|ftp|afs|news|nntp|mid|cid|mailto|" +
                       "wais|prospero|telnet|gopher)";
toXHTML.urlPattern   = toXHTML.urlProtocols + ":" +
                       "[^\\]|\\n]*(?:\\][^\\]|\\n]+)*";
toXHTML.loneURLPattern = "(?:" + toXHTML.urlProtocols +
                         ":[\\$-:=\\?-Z_a-z~]+[\\$-+\\/-Z_a-z~-])";

toXHTML.rawURL = new MarkupRule( "(" + toXHTML.loneURLPattern + ")",
  function(r) {
	console.info(r[2] +" fires rawUrl with href="+r[1]);
    return "<a href=\"" + r[1] + "\">" + r[1] + "</a>";
  }
);
toXHTML.unnamedURL = new MarkupRule(
  "\\[\\[(" + toXHTML.urlPattern + ")\\]\\]",
  function(r) {
	console.info(r[2] +" fires unnamedUrl with href="+r[1]);
    return "<a href=\"" + r[1] + "\">" + r[1] + "</a>";
  }
);
toXHTML.unnamedLink = new MarkupRule(
  "\\[\\[(" + toXHTML.linkPattern + ")\\]\\]",
  function(r) {
	console.info(r[2] +" fires unamedLink with href="+r[1]);
	pagePart = r[1].replace(new RegExp("\\ ", "g"), "_");
    basePart ="/about/";
	hrefStr = basePart + pagePart;
	var clz = "existing";
	//var req = new XMLHttpRequest();
	//req.open('GET', hrefStr, false);
	//req.send(null);
	//if (req.status == 404) 
	//	clz = clz + "new";
	//else
	//	clz = clz + "existing";
	return "<a href=\"" + hrefStr + "\" class=\""+clz+"\" >" + r[1] + "</a>";
  }
);
toXHTML.namedURL = new MarkupRule(
  "\\[\\[(" + toXHTML.urlPattern + ")\\|(.*?)\\]\\]",
  function(r) {
	console.info(r[2] +" fires namedURL with href="+r[1]);
    return "<a href=\"" + r[1] + "\">" + r[2] + "</a>";
  }
);
toXHTML.namedLink = new MarkupRule(
  "\\[\\[(" + toXHTML.linkPattern + ")\\|(.*?)\\]\\]",
  function(r) {
	console.info(r[2] +" fires namedLink with href="+r[1]);
    return "<a href=\"" + r[1] + "\">" + r[2] + "</a>";
  }
);

  // Images
toXHTML.img = new MarkupRule(
  "{{([^|\\n{}][^|\\n}]*(?:}[^|\\n}]+)*)\\|([^|\\n}]*(?:}[^|\\n}]+)*)}}",
  function(r) {
    return "<img src=\"" + r[1] + "\" alt=\"" + r[2] + "\"/>";
  }
);

  // Children of lists
toXHTML.ulist.children = toXHTML.olist.children = [ toXHTML.li ];
toXHTML.li.children = [ toXHTML.olist, toXHTML.ulist, toXHTML.singleLine ];

  // Children of table items
toXHTML.table.children = [ toXHTML.tr ];
toXHTML.tr.children = [ toXHTML.td ];
toXHTML.td.children = [ toXHTML.singleLine ];

  // Children within blocks
toXHTML.singleLine.children = toXHTML.paragraph.children =
  toXHTML.strong.children = toXHTML.em.children = toXHTML.tt.children =
  [ toXHTML.strong, toXHTML.em, toXHTML.br, toXHTML.rawURL,
    toXHTML.unnamedURL, toXHTML.unnamedLink, toXHTML.namedURL,
    toXHTML.namedLink, toXHTML.tt, toXHTML.img ];


  // The root rule used to start the parser
toXHTML.root = new MarkupRule();
toXHTML.root.children          = [ toXHTML.h1, toXHTML.h2, toXHTML.h3,
                                   toXHTML.h4, toXHTML.h5, toXHTML.h6,
                                   toXHTML.hr, toXHTML.olist,
                                   toXHTML.ulist, toXHTML.preBlock,
                                   toXHTML.table ];
toXHTML.root.fallback          = new MarkupRule();
toXHTML.root.fallback.children = [ toXHTML.paragraph ];


//// Do the rendering ////////////////////////////////////////////////////////
// Apply each rule, and use whichever matches first in the text
// If there is a tie, use whichever is first in the list of rules
MarkupRule.prototype.markUp = function(text) {
  var head = "";
  var tail = "" + text;
  var matches = [ ];
  for (var i = 0; i < this.children.length; i++) {
    matches[i] = tail.match(this.children[i].regex);
  }
  var best = false;
  var b_i  = false;
  for (var i = 0; i < this.children.length; i++)
    if (matches[i] && (!best || best.index > matches[i].index)) {
      best = matches[i];
      b_i  = i;
    }
  while (best) {
    if ((best.index > 0) && (this.fallback))
      head += this.fallback.markUp(tail.substring(0,best.index));
    else
      head += tail.substring(0,best.index);
    head += this.children[b_i].rule(best);
    var chopped = best.index + best[0].length;
    tail = tail.substring(chopped);
    for (var i = 0; i < this.children.length; i++)
      if (matches[i])
        if (matches[i].index >= chopped)
          matches[i].index -= chopped;
        else
          matches[i] = tail.match(this.children[i].regex);
    best = false;
    for (var i = 0; i < this.children.length; i++)
      if (matches[i] && (!best || best.index > matches[i].index)) {
        best = matches[i];
        b_i  = i;
      }
  }
  if (tail.length > 0 && this.fallback)
    tail = this.fallback.markUp(tail);
  return head + tail;
}

//// Install the renderer //////////////////////////////////////////////
function updateRender() {
   $("Html").innerHTML = toXHTML($("Text").value);
}

function installRenderer() {
   console.info("Called installRenderer");
   element = $("Text");
   element.onkeyup = element.onkeypress = element.ondrop = element.onchange = updateRender;
   updateRender();
}

window.onload = installRenderer;
