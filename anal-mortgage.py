#!/usr/bin/env python
# coding: utf-8

# In[2]:


import geopandas
import contextily as cx
from math import nan
import geopandas as gpd
from numpy import NaN
import numpy as np
import matplotlib
import pandas as pd
import matplotlib.pyplot as plt
import contextily as ctx
import descartes
from pygeos.set_operations import intersection
from shapely import geometry
from shapely.geometry import Point, Polygon


# In[3]:


df = pd.read_csv('track.csv')


# In[22]:


df.head()


# In[15]:


df['census1'] = df["census_tract"]


# In[16]:


df = df[df.census1.notna()]


# In[18]:


df = df[df.census1.notna()]


# In[19]:


df['census1'] = df['census1'].astype(np.int64)


# In[20]:


df.head()


# In[21]:


df['census1'] = df['census1'].apply(str)


# In[23]:


df.info()


# In[24]:


df.head()


# In[25]:


df['census1'] = df['census1'].str[3:]


# In[26]:


df.head()


# In[27]:


df['census1'] = df['census1'].str[2:]


# In[28]:


df.head()


# In[29]:


df2 = pd.read_csv('kx-houston-texas-family-income-by-census-block-group-2010-CSV/houston-texas-family-income-by-census-block-group-2010.csv')


# In[30]:


df2.head()


# In[31]:


df3 = df2["WKT"]


# In[32]:


df3.head()


# In[34]:


df4 = df2[["WKT", "TRACT"]]


# In[35]:


df4.head()


# In[36]:


df.head()


# In[37]:


df.join(df4.set_index('TRACT'), on='census1')


# In[38]:


df4.info()


# In[39]:


df4['TRACT'] = df4['TRACT'].apply(str)


# In[40]:


df4["TRACT"] = df4.loc["TRACT"].apply


# In[41]:


df4['TRACT'] = df['TRACT'].astype(np.int64)


# In[42]:


d4.head()


# In[43]:


df4()


# In[44]:


df4.head()


# In[46]:


df4["TRACT"]


# In[47]:


df4["TRACT"] = df4["TRACT"].astype(np.int64)


# In[49]:


df4.info()


# In[51]:


df.info()


# In[53]:


df["census1"] = df["census1"].astype(np.int64)


# In[54]:


df.info()


# In[58]:


df7 = df.join(df4.set_index('TRACT'), on='census1')


# In[56]:


df.to_csv()


# In[59]:


df7.to_csv("ugh.csv")


# In[60]:


df7.head()


# In[62]:


gdf = geopandas.GeoDataFrame(df7, geometry='WKT')


# In[63]:


df2.info()


# In[64]:


df7.info()


# In[65]:


df_new = pd.read_csv('ugh.csv')


# In[66]:


gdf_new = geopandas.GeoDataFrame(df_new, geometry='WKT')


# In[69]:


df_new1 = df_new[df_new.WKT.notna()]


# In[70]:


df_new1.head()


# In[71]:


gdf_new11 = geopandas.GeoDataFrame(df_new1, geometry='WKT')


# In[74]:


df_new1['WKT'] = df_new1['WKT'].apply(wkt.loads)


# In[76]:


df_new1['WKT'] = gpd.GeoSeries.from_wkt(df['WKT'])


# In[78]:


NEW = df_new1[["income", "WKT"]]


# In[79]:


NEW.info()


# In[80]:


NEW['geom'] = gpd.GeoSeries.from_wkt(NEW['WKT'])


# In[81]:


my_geo_df = gpd.GeoDataFrame(NEW, geometry='geom')


# In[83]:


print(my_geo_df.head())


# In[85]:


ax = my_geo_df.plot(figsize=(100, 100), alpha=0.5, edgecolor='k')
cx.add_basemap(ax)


# In[96]:


ax = my_geo_df.plot(figsize=(100, 100), column='income', scheme='quantiles', k=7, cmap='OrRd', edgecolor='k', legend=True, alpha=0.1)
cx.add_basemap(ax)
plt.savefig('firstFig3.jpg')


# In[91]:


plt.savefig('firstFig.jpg')


# In[97]:


NEW.head()


# In[98]:


NEW.to_csv("new.csv")


# In[ ]:




