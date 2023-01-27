import icalendar
from icalendar import Calendar, Event
from urllib.request import urlopen

def merge_calendars(feed_urls, link_suffix, link_name=None, anonymize=False, prefix='', suffix=''):
    # Create an empty calendar
    merged_calendar = Calendar()
    merged_calendar.add('version', '2.0')
    merged_calendar.add('prodid', '-//My calendar merger//')
    merged_calendar.add('CALSCALE','GREGORIAN')
    for feed_url in feed_urls:
        # Retrieve the calendar from the feed URL
        feed = urlopen(feed_url)
        calendar = Calendar.from_ical(feed.read())
        for component in calendar.subcomponents:
            if component.name == "VEVENT":
                event = Event.from_ical(str(component))
                if anonymize:
                    event.add('SUMMARY', 'Anonymized Event')
                    event.add('LOCATION', '')
                    event.add('DESCRIPTION', '')
                else:
                    event.add('SUMMARY', prefix + event['SUMMARY'] + suffix)
                # Append the event to the merged calendar
                merged_calendar.add_component(event)
    # Generate the merged link
    if link_name:
        merged_link = f"https://example.com/{link_name}/{link_suffix}"
    else:
        merged_link = f"https://example.com/{link_suffix}"
    return merged_link, merged_calendar.to_ical()
