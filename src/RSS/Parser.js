var RssParser = require('rss-parser');
var parser = new RssParser({
  headers: {
    'Accept': 'text/html,application/xhtml+xml,application/xml,application/rss+xml;q=0.9,*/*;q=0.8',
    'Accept-Language': 'en-us,en;q=0.5',
    'Accept-Encoding': 'gzip,deflate',
    'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.7',
  },
});

exports._parseURL = function(url) {
  return function(reject, resolve) {
    return parser.parseURL(url).then(function(feed) {
      return resolve({
        title: feed.title || '',
        url: feed.feedUrl || '',
        description: feed.description || '',
        items: (feed.items || []).map(function(item) {
          return {
            title: item.title || '',
            author: item.author || '',
            url: item.link || '',
            content: item.content || '',
            snippet: item.contentSnippet || '',
            date: item.pubDate || '',
          };
        }),
      });
    }).catch(reject);
  };
};
