import scrapy


class GtrSpider(scrapy.Spider):
    name = "gtr"
    allowed_domains = ["gtr.ukri.org"]
    start_urls = ["https://gtr.ukri.org"]

    def parse(self, response):
        pass
