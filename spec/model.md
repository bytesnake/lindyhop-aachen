# Event
An event has
- a name,
- a teaser,
- a description
- a list of occurrences.

This part of the model is mainly used for information that helps people figure out what the event is about. The teaser is a short description that can be shown alongside the event, whereas the description is longer and contains all relevant information.

An occurrence is the actual happening of an event. An event can occur only once, or multiple times, therefore it has a list of occurrences.

# Occurrence
An occurrence has
- a starting date and time,
- a duration,
- a location.

> We chose a duration instead of an end date-time, because that would have an illegal configuration where the end date-time is before the starting date-time. With this model, this case is impossible.
>
> Note, however, that the user interface should let the user fix the end date-time.

# Location
A location has
- a name,
- an address.
