#!/usr/bin/env python
# coding: utf-8

# In[1]:


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


# In[2]:


df2 = pd.read_csv('ugh.csv')


# df2.head()

# In[3]:


df2.head()


# In[4]:


df3 = df2[["census1", "accepted"]]
df_geom = df2[["census1", "WKT"]]


# In[5]:


df3.head()


# In[6]:


df3 = df3.groupby(['census1']).mean()


# In[7]:


df3.head()


# In[8]:


df_geom.head()


# In[10]:


df_geom.info()


# In[9]:


df3.info()


# In[42]:


df_joint = df3.merge(df_geom, left_on = 'census1', right_on='census1', how = 'inner')


# In[43]:


df_joint.head()


# In[45]:


df_joint.info()


# In[46]:


df_joint = df_joint[df_joint.WKT.notna()]


# In[47]:


df_joint.head()


# In[48]:


df_joint.info()


# In[49]:


df_joint.to_csv("hope2.csv")


# In[50]:


new = pd.read_csv('hope2.csv')


# In[51]:


new.head()


# In[52]:


new = new[["accepted", "WKT"]]


# In[53]:


new.head()


# In[54]:


new['geom'] = gpd.GeoSeries.from_wkt(new['WKT'])


# In[55]:


my_geo_df = gpd.GeoDataFrame(new, geometry='geom')


# In[56]:


my_geo_df.head()


# In[60]:


ax = my_geo_df.plot(figsize=(20, 20), column='accepted', scheme='quantiles', k=5, cmap='OrRd', edgecolor='k', legend=True, alpha=0.4)
cx.add_basemap(ax)
plt.savefig('otherFigNEW_STEVO4.jpg')


# In[ ]:




