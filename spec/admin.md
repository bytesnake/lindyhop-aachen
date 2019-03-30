# Occurrence Generator
Many events differ only in the date, and even repeat bi-weekly. A generator simplifies the insertion of such regular events.

It allows to set starting time, duration, and location. Multiple dates can be selected in a calendar view. It then adds occurrences with the set attributes for all the selected dates.

# Authentication
The website does not need the highest degree of security. Logins should be stored on the device for a month, and refreshed with each request, in order to minimize the friction of login on the user.

Authentication is based on cookies. The server offers a route to verify whether the authentication cookie is set and valid. Since clients need to check whether a valid cookie is present, the server offers a route that returns whether the request contained a valid cookie.