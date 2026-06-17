SELECT
  cs.cruise_id,
  cs.latitude,
  cs.longitude,
  TO_NUMBER(TO_CHAR(cs.cruise_date, 'YYYY')) year,
  TO_NUMBER(TO_CHAR(cs.cruise_date, 'MM')) month,
  TO_NUMBER(TO_CHAR(cs.cruise_date, 'DD')) day,
  cs.cruise_time,
  cs.datatype,
  cm.pressure,
  cm.temperature,
  cm.salinity,
  cs.maximum_depth,
  cs.flag,
  cm.stn_id
FROM
    climate.stations cs,
    climate.measurements cm
WHERE
    cs.stn_id = cm.stn_id
    AND cs.longitude BETWEEN -66.9 AND -66.8
    AND cs.latitude BETWEEN 44.9 AND 45.0
    ORDER BY
    year,
    month,
    day,
    cs.longitude