import pandas as pd
import streamlit as st
import pandas as pd
import pydeck as pdk
import time
import math

# Load data
filename = 'gold_time_series.csv'
df = pd.read_csv(filename)
df["radius"] = df["gold_medals"].apply(lambda gold_medals: gold_medals*3000)


# Title for map
map_title = ('Olympic gold medals by country')
st.subheader(map_title)

# Subheading to display current date
Year = 1896
timestamp = st.subheader(str(Year))

# Create view
# Set viewport for deckgl map
view = pdk.ViewState(latitude=0, longitude=0, zoom=0.2,)

# Create scatter plot layer
goldLayer = pdk.Layer(
        "ScatterplotLayer",
        data=df,
        pickable=True,
        opacity=0.8,
        stroked=True,
        filled=True,
        radius_scale=6,
        radius_min_pixels=1,
        radius_max_pixels=20,
        line_width_min_pixels=1,
        get_position=["Longitude", "Latitude"],
        get_radius="radius",
        get_fill_color=[206,18,86],
        get_line_color=[206,18,86],
    )

tooltip_text = {"html": "{country} won {gold_medals} gold medals in {Year}"}

# Create deck.gl map
r = pdk.Deck(
    layers=goldLayer,
    initial_view_state=view,
    map_style="mapbox://styles/mapbox/light-v10",
    tooltip=tooltip_text
)
# Render deck.gl map in Streamlit app as Pydeck chart 
map = st.pydeck_chart(r)

# Update maps and subheading each 4 years from 1869 to 2016
for i in range(1896, 2016, 4):
    # Increase day by 1
    Year += 4

    # Update data in map layers
    goldLayer.data = df[df['Year'] == Year]
    
    # Update the deck.gl map
    r.update()

    # Render the map
    map.pydeck_chart(r)

    # Update heading with current date
    timestamp.subheader(Year)

    # wait 0.5 second before advancing by four years
    time.sleep(0.5)


