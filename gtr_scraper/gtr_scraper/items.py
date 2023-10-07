import scrapy

class MyProjectItem(scrapy.Item):
    url = scrapy.Field()
    abstract = scrapy.Field()
    impact = scrapy.Field()