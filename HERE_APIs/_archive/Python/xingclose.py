def xingCloseRoute(name, point, offset, buffer, rail):
    
    # Buffer and intersect points with rail
    navPoints = point.buffer(buffer).intersection(rail).unary_union
    
    # Convert points to dataframe
    coords_df = pd.DataFrame(list(navPoints.coords), columns=['x', 'y'])
    
    # Perform k-means clustering
    kmeans = KMeans(n_clusters=2, random_state=123).fit(coords_df)
    
    # Check if k-means returned enough centers
    if len(kmeans.cluster_centers_) < 2:
        raise ValueError("k-means did not return enough centers.")
    
    # Calculate angle
    angle = math.atan2(kmeans.cluster_centers_[1,1] - kmeans.cluster_centers_[0,1], kmeans.cluster_centers_[1,0] - kmeans.cluster_centers_[0,0]) + math.pi/2
    
    # Calculate side points
    points_df = pd.DataFrame({
        'point': ['side1', 'side2'],
        'x': [np.mean(kmeans.cluster_centers_[:,0]) + np.cos(angle) * offset, np.mean(kmeans.cluster_centers_[:,0]) - np.cos(angle) * offset],
        'y': [np.mean(kmeans.cluster_centers_[:,1]) + np.sin(angle) * offset, np.mean(kmeans.cluster_centers_[:,1]) - np.sin(angle) * offset]
    })
    sidePoints = gpd.GeoDataFrame(points_df, geometry=gpd.points_from_xy(points_df.x, points_df.y), crs=crs)
    
    # Create barrier
    barrier = navPoints.buffer(10).unary_union
    
    # Perform routing
    routeTest1 = ors.directions(
        coordinates=[(sidePoints.loc[0, 'x'], sidePoints.loc[0, 'y']), (sidePoints.loc[1, 'x'], sidePoints.loc[1, 'y'])],
        profile='driving-car',
        format='geojson',
        avoid_polygons=[barrier]
    )
    routeTest2 = ors.directions(
        coordinates=[(sidePoints.loc[1, 'x'], sidePoints.loc[1, 'y']), (sidePoints.loc[0, 'x'], sidePoints.loc[0, 'y'])],
        profile='driving-car',
        format='geojson',
        avoid_polygons=[barrier]
    )
    
    # Convert routes to GeoDataFrame
    routes = pd.concat([gpd.GeoDataFrame(routeTest1['features'][0]['geometry'], crs=crs), gpd.GeoDataFrame(routeTest2['features'][0]['geometry'], crs=crs)]).reset_index(drop=True)
    routes['name'] = name
    routes = routes[['name', 'geometry']]
    
    return routes
