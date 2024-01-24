


TarxienCore <- function(fileName,
                        runstars = FALSE,
                        runsun = TRUE,
                        cycle = FALSE,
                        maxError = 5,
                        fastMode = FALSE,
                        yearRange = c()) {
  packages <- c("gridExtra", "grid")

  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  invisible(lapply(packages, library, character.only = TRUE))

  library("gridExtra")
  library("grid")
  options(scipen = 100)
  options(digits = 16)
  recording <<- FALSE
  filename <<- ""
  sitelist = read.csv(fileName)

  if (length(yearRange) < 1) {
    yearRange = c(as.numeric(sitelist[a, 8]))
  }

  a = 1
  while (a < nrow(sitelist) + 1) {
    RunSite(
      id = sitelist[a, 1],
      latitude = as.numeric(sitelist[a, 2]),
      longitude = as.numeric(sitelist[a, 3]),
      altitude = as.numeric(sitelist[a, 4]),
      horizon = as.numeric(sitelist[a, 5]),
      ori = as.numeric(sitelist[a, 6]),
      date = as.numeric(sitelist[a, 7]),
      year = as.numeric(sitelist[a, 8]),
      time = as.numeric(sitelist[a, 9]),
      max = as.numeric(sitelist[a, 10]),
      runstars1 = runstars,
      runsun = runsun,
      cycle = cycle,
      maxError = maxError,
      fastMode = fastMode,
      yearRange = yearRange
    )
    a = a + 1
  }

  if (cycle == TRUE){
  StarTable()
  }
}

RunSite <-
  function(id,
           longitude,
           latitude,
           altitude,
           horizon,
           ori,
           max,
           date,
           year,
           time,
           runstars1,
           runsun,
           cycle,
           maxError,
           fastMode,
           yearRange) {
    startwd = getwd()

    dir.create(paste0(getwd(), "/", id), showWarnings = FALSE)

    if (runsun == TRUE) {
      sumsol <<- 0
      winsol <<- 0
      sumsolday <<- 0
      winsolday <<- 0
    }
    if (cycle == FALSE) {
      tempdata = ParseDay(date, time)

      jde = ConvertDateToJD(
        day = tempdata[1],
        month = tempdata[2],
        hour = tempdata[3],
        minute = tempdata[4],
        year = year
      )
      fileToSave <<- data.frame()
      if (runsun == TRUE) {
        SunCalculations(
          id = id,
          longitude = longitude,
          latitude = latitude,
          altitude = altitude,
          horizon = horizon,
          ori = ori,
          max = max,
          jde = jde,
          cycle = FALSE,
          date = date,
          year = year,
          time = time,
          maxError = maxError,
          month = 0,
          day = 0
        )
      }

      if (runstars1 == TRUE) {
        starlist = read.csv("stars.csv")

        b = 1
        fileToSave <<- data.frame()
        while (b < nrow(starlist) + 1) {
          StarsCalculations(
            id = id,
            longitude = longitude,
            latitude = latitude,
            altitude = altitude,
            horizon = horizon,
            ori = ori,
            max = max,
            jde = jde,
            starNum = b,
            cycle = FALSE,
            date = date,
            year = year,
            time = time,
            maxError = maxError
          )
          b = b + 1
        }
      }
    } else if (cycle == TRUE) {
      for (i in yearRange) {
        year = i
        starlist = read.csv("stars.csv")
        if (runstars1 == TRUE) {
          b = 1
        } else {
          b = nrow(starlist)
        }
        while (b < nrow(starlist) + 1) {
          fileToSave <<- data.frame()
          print(" ")
          if (runstars1 == TRUE) {
            print(paste0("Running ", starlist[b, 1], " for ", id, " in ", year))
          } else {
            print(paste0("Running Sun for ", id, " in ", year))
          }
          pb = txtProgressBar(
            min = 0,
            max = 100,
            style = 3,
            width = 50,
            char = "="
          )
          month = 1
          while (month < 13) {
            day = 1
            if (month == 4 ||
                month == 6 || month == 9 || month == 11) {
              maxDays = 30
            } else if (month == 2) {
              maxDays = 28
            } else {
              maxDays = 31
            }
            while (day < maxDays + 1) {
              hour = 0
              while (hour < 24) {
                minute = 0
                while (minute < 60) {
                  tempResults = c()
                  jde = ConvertDateToJD(
                    day = day,
                    month = month,
                    hour = hour,
                    minute = minute,
                    year = year
                  )

                  if (runsun == TRUE) {
                    tempResults = SunCalculations(
                      id = id,
                      longitude = longitude,
                      latitude = latitude,
                      altitude = altitude,
                      horizon = horizon,
                      ori = ori,
                      max = max,
                      jde = jde,
                      cycle = TRUE,
                      date = paste0(day, "/", month),
                      year = year,
                      time = paste0(hour, ":", minute),
                      maxError = maxError,
                      month = month,
                      day = day
                    )
                  }

                  if (runstars1 == TRUE) {
                    tempResults = StarsCalculations(
                      id = id,
                      longitude = longitude,
                      latitude = latitude,
                      altitude = altitude,
                      horizon = horizon,
                      ori = ori,
                      max = max,
                      jde = jde,
                      starNum = b,
                      cycle = TRUE,
                      date = paste0(day, "/", month),
                      year = year,
                      time = paste0(hour, ":", minute),
                      maxError = maxError
                    )
                  }
                  if (fastMode == TRUE &&
                      minute == 0 && recording == FALSE && (
                        abs(ori - Normalise(tempResults[2])) > maxError + 15 ||
                        tempResults[1] < -15 ||
                        tempResults[1] > max + 15
                      )) {
                    minute = 60

                  } else{
                    minute = minute + 1
                  }
                }
                if (fastMode == TRUE) {
                  if (runstars1 == TRUE && hour == 7 && recording == FALSE) {
                    hour = 18
                  } else if (runsun == TRUE &&
                             hour < 5 && recording == FALSE) {
                    hour = 5
                  } else if (runsun == TRUE &&
                             hour > 10 && recording == FALSE) {
                    hour = 25
                  } else {
                    hour = hour + 1
                  }
                } else {
                  hour = hour + 1
                }
              }
              day = day + 1
            }
            setTxtProgressBar(pb, floor((100 / 12) * month))
            month = month + 1
          }
          if (runsun == TRUE) {
            print(paste0(sumsolday, "  ", winsolday))
          }

          setwd(paste0(getwd(), "/", id))
          CycleSave()
          setwd(startwd)
          b = b + 1
        }
      }
      setwd(paste0(getwd(), "/", id))
      if (runstars1 == TRUE){
      FinalFiles()
      }
      setwd(startwd)
    }
  }

StarsCalculations <-
  function(id,
           longitude,
           latitude,
           altitude,
           horizon,
           ori,
           max,
           jde,
           starNum,
           cycle,
           date,
           time,
           year,
           maxError) {
    starlist = read.csv("stars.csv")
    name = starlist[starNum, 1]
    starAscension = starlist[starNum, 2]
    starDeclination = starlist[starNum, 3]
    starMotAsc = starlist[starNum, 4]
    starMotDec = starlist[starNum, 5]

    t = (jde - 2451545) / 36525
    starAscension = starAscension + (starMotAsc * t * 100)
    starDeclination = starDeclination + (starMotDec * t * 100)
    trueAxis = CalculateEarthAxis(jde)

    t2 = ((jde - 2451545) / 36525) / 10000
    earthAxis = 23.43929 - 1.300258 * t2 - 0.0004305556 * (t2 ^ 2) + 0.5553472 *
      (t2 ^ 3) - 0.01427222 * (t2 ^ 4) - 0.06935278 * (t2 ^ 5) - 0.01084722 *
      (t2 ^ 6) +
      0.001977778 * (t2 ^ 7) + 0.007741667 * (t2 ^ 8) + 0.001608333 * (t2 ^
                                                                         9) +
      0.0006805556 * (t2 ^ 10)

    omega = Normalise(125.04452 - (1934.136261 * t))
    sunLon = Normalise(280.4665 + (36000.7698 * t))
    moonLon = Normalise(218.3165 + (481267.8813 * t))

    deltaPsi = (-0.004777778 * sin(Rad2Deg(omega))) - (0.0003666667 *
                                                         sin(Rad2Deg (2 * sunLon))) - (0.00006388889 * sin(Rad2Deg(2 *
                                                                                                                     moonLon))) + (0.00005833333 * sin(Rad2Deg(2 * omega)))
    deltaEpsilon = (0.002555556 * cos(Rad2Deg(omega))) + (0.0001583333 *
                                                            cos(Rad2Deg(2 * sunLon))) + (0.00002777778 * cos(Rad2Deg(2 *
                                                                                                                       moonLon))) - (0.000025 * cos(Rad2Deg(2 * omega)))

    starEta = (2306.2181 * t + 0.30188 * (t ^ 2) + 0.017998 * (t ^ 3)) / 3600
    starZeta = (2306.2181 * t + 1.09468 * (t ^ 2) + 0.018203 * (t ^ 3)) / 3600
    starTheta = (2004.3109 * t - 0.42665 * (t ^ 2) - 0.041833 * (t ^ 3)) / 3600

    aa = cos(Deg2Rad(starDeclination)) * sin(Deg2Rad
                                             (starAscension + starEta))
    bb = cos(Deg2Rad(starTheta)) * cos(Deg2Rad(starDeclination)) *
      cos(Deg2Rad(starAscension + starEta)) - sin(Deg2Rad(starTheta)) *
      sin(Deg2Rad(starDeclination))
    cc = sin(Deg2Rad(starTheta)) * cos(Deg2Rad(starDeclination)) *
      cos(Deg2Rad(starAscension + starEta)) + cos(Deg2Rad(starTheta)) *
      sin(Deg2Rad(starDeclination))

    starAscension = Rad2Deg(atan2(aa, bb)) + starZeta
    starDeclination = Rad2Deg(asin(cc))

    apparentAsc = starAscension + deltaPsi * (cos(Deg2Rad(trueAxis)) +
                                                sin(Deg2Rad(trueAxis)) * sin(Deg2Rad(starAscension)) *
                                                tan(Deg2Rad(starDeclination))) - deltaEpsilon *
      (cos(Deg2Rad(starAscension)) * tan(Deg2Rad(starDeclination)))
    apparentDec = starDeclination + deltaPsi * (sin(Deg2Rad(trueAxis)) *
                                                  cos(Deg2Rad(starAscension))) + deltaEpsilon * (sin(Deg2Rad
                                                                                                     (starAscension)))

    tempMeanLongitude = Normalise(280.46645 + 36000.76983 * t +
                                    0.0003032 * (t ^ 2))
    meanAnomaly = Normalise(357.52910 + 35999.05030 * t -
                              0.0001559 * (t ^ 2) - 0.00000048 * (t ^ 3))
    sunCentre = (1.914600 - 0.004817 * t - 0.000014 * (t ^ 2)) *
      sin(Deg2Rad(meanAnomaly)) + (0.019993 - 0.000101 * t) * sin(Deg2Rad
                                                                  (2 * meanAnomaly)) + 0.000290 * sin(Deg2Rad(3 * meanAnomaly))
    trueGeoLong = tempMeanLongitude + sunCentre
    k = 0.0056932
    e = 0.016708617 - 0.000042037 * t - 0.0000001236 * (t ^ 2)
    pi = 102.93735 + 1.71953 * t + 0.00046 * (t ^ 2)

    apparentAsc = apparentAsc - k * ((
      cos(Deg2Rad(starAscension)) *
        cos(Deg2Rad(trueGeoLong)) * cos(Deg2Rad(trueAxis)) + sin(Deg2Rad
                                                                 (starAscension)) * sin(Deg2Rad(trueGeoLong))
    ) / cos(Deg2Rad
            (starDeclination))) + e * k * ((
              cos(Deg2Rad(starAscension)) *
                cos(Deg2Rad(pi)) * cos(Deg2Rad(trueAxis)) + sin(Deg2Rad
                                                                (starAscension)) * sin(Deg2Rad(pi))
            ) / cos(Deg2Rad(starDeclination)))
    apparentDec = apparentDec - k * (
      cos(Deg2Rad(trueGeoLong)) *
        cos(Deg2Rad(trueAxis)) * (tan(Deg2Rad(trueAxis)) * cos(Deg2Rad
                                                               (starDeclination)) - sin(Deg2Rad(starAscension)) * sin(Deg2Rad
                                                                                                                      (starDeclination))) + cos(Deg2Rad(starAscension)) * sin(Deg2Rad
                                                                                                                                                                              (starDeclination)) * sin(Deg2Rad(trueGeoLong))
    ) + e * k * (
      cos(Deg2Rad
          (pi)) * cos(Deg2Rad(trueAxis)) * (tan(Deg2Rad(trueAxis)) *
                                              cos(Deg2Rad(starDeclination)) - sin(Deg2Rad(starAscension)) *
                                              sin(Deg2Rad(starDeclination))) + cos(Deg2Rad(starAscension)) *
        sin(Deg2Rad(starDeclination)) * sin(Deg2Rad(pi))
    )

    starAscension = apparentAsc
    starDeclination = apparentDec

    starDeclination = Normalise(starDeclination)

    greenTime = Normalise(
      280.46061837 + (360.98564736629 * (jde - 2451545)) +
        (0.000387933 * (t ^ 2)) - ((t ^ 3) / 38710000) + (deltaPsi * cos(Deg2Rad(trueAxis)))
    )
    hourAngle = Normalise(greenTime - longitude - starAscension)
    hourAngle = Normalise(hourAngle)
    starAzimuth = Rad2Deg(atan2(
      sin(Deg2Rad(hourAngle)),
      cos(Deg2Rad
          (hourAngle)) * sin(Deg2Rad(latitude)) - tan(Deg2Rad
                                                      (starDeclination)) * cos(Deg2Rad(latitude))
    )) + 180
    starAltitude = Rad2Deg (asin(
      sin(Deg2Rad(latitude)) * sin(Deg2Rad
                                   (starDeclination)) + cos(Deg2Rad(latitude)) * cos(Deg2Rad
                                                                                     (starDeclination)) * cos(Deg2Rad(hourAngle))
    ))

    starAzimuth = Normalise(starAzimuth)


    altitude2 = CalculateRefraction(starAltitude)

    SaveFiles(
      id = id,
      starNum = starNum,
      azi = starAzimuth,
      alt = altitude2,
      ori = ori,
      cycle = cycle,
      max = max,
      date = date,
      year = year,
      time = time,
      horizon = horizon,
      maxError = maxError
    )

    return(c(altitude2, starAzimuth))


  }


SunCalculations <- function(id,
                            longitude,
                            latitude,
                            altitude,
                            horizon,
                            ori,
                            max,
                            jde,
                            cycle,
                            date,
                            time,
                            year,
                            maxError,
                            month,
                            day) {
  jDate = (jde - 2451545) / 36525

  trueAxis = CalculateEarthAxis(jde)

  t = jDate
  meanLongitude = Normalise(280.46646 + 36000.76983 * t + 0.0003032 * (t ^
                                                                         2))
  meanAnomaly = Normalise(357.52910 + 35999.05030 * t -
                            0.0001559 * (t ^ 2) - 0.00000048 * (t ^
                                                                  3))
  earthEccentricity = 0.016708617 - 0.000042037 * t - 0.0000001236 * (t ^
                                                                        2)
  sunCentre = (1.914600 - 0.004817 * t - 0.000014 * (t ^ 2)) * sin(Deg2Rad(meanAnomaly)) +
    (0.019993 - 0.000101 * t) * sin(Deg2Rad(2 * meanAnomaly)) +
    0.000290 * sin(Deg2Rad(3 * meanAnomaly))
  sunTrueLongitude = meanLongitude + sunCentre
  trueAnomaly = meanAnomaly + sunCentre
  sunRadius = (1.000001018 * (1 - (earthEccentricity ^ 2))) /
    (1 + earthEccentricity * cos(Deg2Rad(trueAnomaly)))
  omega = 125.04 - 1934.136 * t
  apparentLongitude = Normalise(sunTrueLongitude - 0.00569 - 0.00478 * sin(Deg2Rad(omega)))


  rightAscension = Rad2Deg(atan2(cos(Deg2Rad(
    trueAxis + (0.00256 * cos(Deg2Rad(omega)))
  )) *
    sin(Deg2Rad(
      apparentLongitude
    )), cos(Deg2Rad(
      apparentLongitude
    ))))
  apparentAscension = Normalise(rightAscension)
  declination = asin(sin(Deg2Rad(trueAxis)) * sin(Deg2Rad(sunTrueLongitude)))
  apparentDeclination = Rad2Deg(asin(sin(
    Deg2Rad(trueAxis +
              Deg2Rad(0.00256 * cos(Deg2Rad(
                omega
              )))) *
      sin(Deg2Rad(apparentLongitude))
  )))

  if (month > 0) {
    if (apparentDeclination > sumsol) {
      sumsol <<- apparentDeclination
      sumsolday <<- paste0(day, "/", month)
    }
    if (apparentDeclination < winsol) {
      winsol <<- apparentDeclination
      winsolday <<- paste0(day, "/", month)
    }
  }


  omega = Normalise(125.04452 - (1934.136261 * t))
  sunLon = Normalise(280.4665 + (36000.7698 * t))
  moonLon = Normalise(218.3165 + (481267.8813 * t))

  deltaPsi = (-0.004777778 * sin(Rad2Deg(omega))) - (0.0003666667 * sin(Rad2Deg(2 * sunLon))) -
    (0.00006388889 * sin(Rad2Deg(2 * moonLon))) + (0.00005833333 * sin(Rad2Deg(2 * omega)))
  deltaEpsilon = (0.002555556 * cos(Rad2Deg(omega))) + (0.0001583333 * cos(Rad2Deg(2 * sunLon))) +
    (0.00002777778 * cos(Rad2Deg(2 * moonLon))) - (0.000025 * cos(Rad2Deg(2 * omega)))

  greenTime = Normalise(
    280.46061837 + 360.98564736629 * (jde - 2451545) + 0.000387933 * (jDate ^
                                                                        2) -
      (jDate ^ 3) / 38710000 + deltaPsi * cos(Deg2Rad(trueAxis))
  )
  t = jDate / 10
  newMeanLon = Normalise(
    280.4664567 + (360007.6982279 * t) + (0.03032028 * (t ^ 2)) +
      ((t ^ 3) / 49931) - ((t ^ 4) / 15299) - ((t ^
                                                  5) / 1988000)
  )
  equationOfTime = Normalise(newMeanLon - apparentAscension + deltaPsi * cos(Deg2Rad(trueAxis)))
  hourAngle = Normalise(greenTime - longitude - apparentAscension + equationOfTime)


  azimuth = Rad2Deg(atan2(
    sin(Deg2Rad(hourAngle)),
    cos(Deg2Rad(hourAngle)) *
      sin(Deg2Rad(latitude)) - tan(Deg2Rad(apparentDeclination)) * cos(Deg2Rad(latitude))
  )) + 180
  altitude = Rad2Deg(asin(
    sin(Deg2Rad(latitude)) * sin(Deg2Rad(apparentDeclination)) +
      cos(Deg2Rad(latitude)) * cos(Deg2Rad(apparentDeclination)) * cos(Deg2Rad(hourAngle))
  ))


  altitude = CalculateRefraction(altitude)

  azimuth = Normalise(azimuth)

  SaveFiles(
    id = id,
    starNum = 99,
    azi = azimuth,
    alt = altitude,
    ori = ori,
    cycle = cycle,
    max = max,
    date = date,
    year = year,
    time = time,
    horizon = horizon,
    maxError = maxError
  )


  return(c(altitude, azimuth))

}

ConvertDateToJD <-
  function(day, month, hour, minute, year) {
    tempDay = day + (hour / 24) + ((minute / 60) / 24)
    tempYear = year
    if (month > 2)
    {
      tempMonth = month
    } else
    {
      tempYear = tempYear - 1
      tempMonth = month + 12
    }
    if (year > 1582) {
      a = floor(tempYear / 100)
      b = 2 - a + floor(a / 4)
      jdeTemp = floor(365.25 * (tempYear + 4716)) +
        floor(30.6001 * (tempMonth + 1)) + tempDay + b - 1524.5
    }
    else if (year < 1582)
    {
      y = tempYear
      d = floor(y / 100) - floor(y / 400) - 2

      jdeTemp = floor(365.25 * (tempYear + 4716)) +
        floor(30.6001 * (tempMonth + 1)) + (tempDay - d) - 1524.5
    }
    else
    {
      jdeTemp = 0
    }
    return(jdeTemp)
  }

Deg2Rad <- function(deg) {
  rad = (deg * pi) / 180
  return(rad)
}

Rad2Deg <- function(rad) {
  deg = (rad * 180) / pi
  return(deg)
}

CalculateRefraction <- function (alt) {
  return(alt + (1.02 / Rad2Deg(tan(Deg2Rad
                                   (
                                     alt + (10.3 / (alt + 5.11))
                                   )))))
}

CalculateEarthAxis <- function(jde) {
  t = ((jde - 2451545) / 36525) / 100
  earthAxis = 23.43929 - 1.300258 * t - 0.0004305556 * (t ^ 2) + 0.5553472 *
    (t ^ 3) - 0.01427222 * (t ^ 4) - 0.06935278 * (t ^ 5) - 0.01084722 *
    (t ^ 6) +
    0.001977778 * (t ^ 7) + 0.007741667 * (t ^ 8) + 0.001608333 * (t ^
                                                                     9) +
    0.0006805556 * (t ^ 10)
  t2 = (jde - 2451545) / 36525
  omega = Normalise(125.04452 - (1934.136261 * t2))
  sunLon = Normalise(280.4665 + (36000.7698 * t2))
  moonLon = Normalise(218.3165 + (481267.8813 * t2))

  deltaPsi = (-0.004777778 * sin(Rad2Deg(omega))) - (0.0003666667 *
                                                       sin(Rad2Deg (2 * sunLon))) - (0.00006388889 * sin(Rad2Deg(2 *
                                                                                                                   moonLon))) + (0.00005833333 * sin(Rad2Deg(2 * omega)))
  deltaEpsilon = (0.002555556 * cos(Rad2Deg(omega))) + (0.0001583333 *
                                                          cos(Rad2Deg(2 * sunLon))) + (0.00002777778 * cos(Rad2Deg(2 *
                                                                                                                     moonLon))) - (0.000025 * cos(Rad2Deg(2 * omega)))
  trueAxis = earthAxis + deltaEpsilon
  return(trueAxis)
}

Normalise <- function(input) {
  input = input %% 360
  while (input < 0) {
    input = input + 360
  }
  while (input > 360) {
    input = input - 360
  }
  return(input)
}

ParseDay <- function(date, time) {
  if (nchar(date) == 3) {
    day = as.numeric(substr(as.character(date), 1, 1))
    month = as.numeric(substr(as.character(date), 2, 3))
  } else{
    day = as.numeric(substr(as.character(date), 1, 2))
    month = as.numeric(substr(as.character(date), 3, 4))
  }
  if (nchar(time) == 3) {
    hour = as.numeric(substr(as.character(time), 1, 1))
    minute = as.numeric(substr(as.character(time), 2, 3))
  } else{
    hour = as.numeric(substr(as.character(time), 1, 2))
    minute = as.numeric(substr(as.character(time), 3, 4))
  }
  return(c(day, month, hour, minute))
}

SaveFiles <- function(id,
                      starNum,
                      azi,
                      alt,
                      ori,
                      cycle,
                      max,
                      date,
                      year,
                      time,
                      horizon,
                      maxError) {
  difference = ori - Normalise(azi)

  name = ""
  starlist = read.csv("stars.csv")

  if (cycle == FALSE) {
    starlist = read.csv("stars.csv")
    if (starNum == 99) {
      name = "Sun"
    } else {
      name = starlist[starNum, 1]
    }
    fileToSave <<- rbind(fileToSave, c(name, azi, alt, difference))
    if (starNum == nrow(starlist)) {
      colnames(fileToSave) = c("Star", "Azimuth", "Altitude", "Orientation Diff.")

      write.csv(file = paste0(paste(id, date, year, time, sep = "_"), ".csv"), fileToSave)
    }
  } else{
    if (alt > horizon && alt < max && difference < maxError && difference > - maxError) {
      if (recording == FALSE) {
        tempDate <<- date
        tempTime <<- time
        tempDiff <<- difference
        tempAzi <<- azi
        tempAlt <<- alt
        recording <<- TRUE
      }
    } else if (recording == TRUE) {
        if (starNum != 99) {
          name = starlist[starNum, 1]
        } else {
          name = "Sun"
        }
        tempfile3 = c(
          name,
          year,
          id,
          tempDate,
          tempTime,
          tempDiff,
          tempAzi,
          tempAlt,
          date,
          time,
          difference,
          azi,
          alt
        )
        fileToSave <<- rbind(fileToSave, tempfile3)
        filename <<-
          paste0(paste(id, year, name, sep = "_"), ".csv")
        recording <<- FALSE
      }
    }
}

CycleSave <- function() {

  if (nrow(fileToSave) > 1 && ncol(fileToSave) == 13) {
    colnames(fileToSave) = c(
      "Star",
      "Year",
      "Site",
      "Start Date",
      "Start Time",
      "Orientation Diff.",
      "Start Azi.",
      "Start Alt.",
      "End Date",
      "End Time",
      "Orientation Diff.",
      "End Azi.",
      "End Alt."
    )
    AnalyseYear(fileToSave, filename)
    if (fileToSave[1,1] == "Sun"){
      #sprequi = paste0(floor(((as.numeric(substr(sumsolday, 1, 2)) +
      #                         as.numeric(substr(sumsolday, 1, 2)))/2) + 0.5),
      #                "/3")
      #autequi = paste0(floor(((as.numeric(substr(sumsolday, 1, 2)) +
      #                    as.numeric(substr(sumsolday, 1, 2)))/2) - 0.5),
      #                    "/9")
      sprequi = "21/3"
      autequi = "21/9"
      dates = c(sprequi, sumsolday, autequi, winsolday)
      datenames = c("spring_equinox", "summer_solstice", "autumn_equinox", "winter_solstice")

      solsticefile = data.frame()
       a = 1
      while(a < length(dates) + 1){

        if (any(as.character(fileToSave[,4]) == as.character(dates[a]))){
          temprow = fileToSave[as.character(fileToSave[,4]) == as.character(dates[a]),]
          rownames(temprow[a]) = datenames[a]
          if (length(solsticefile) == 0){
            solsticefile = temprow
          } else {
          solsticefile = rbind(solsticefile, temprow)
          }

          write.csv(file = paste0(substr(filename, 1, (nchar(filename) - 4)),"_solequi.csv"), x = solsticefile)
        }
        a = a + 1
      }
  }

    write.csv(file = filename, x = fileToSave)
  }
}

AnalyseYear <- function(datafile, name) {
  tempdf = datafile[, c(4, 6, 11, 5)]
  a = 1
  meanvalue = 0
  while (a < nrow(tempdf)) {
    meanvalue = meanvalue + as.numeric(tempdf[a, 2])
    tempdf[a, 2] = abs((as.numeric(tempdf[a, 2]) + as.numeric(tempdf[a, 3])) /
                         2)

    a = a + 1
  }

  meanvalue = meanvalue / nrow(tempdf)
  tempdf = tempdf[, c(1, 2, 4)]

  pdf(file = (paste0(substr(
    name, 1, (nchar(name) - 4)
  ), ".pdf")))

  plot(
    as.POSIXct(x = tempdf[, 3], format = "%H:%M") ~ as.Date(tempdf[, 1],
                                                            format = "%d/%m"),
    type = "l",
    xlab = "Date",
    ylab = "Start time"
  )


  newname = gsub("_", " ", substr(name, 1, (nchar(name) - 4)))

  title(main = newname,
        sub = paste0("Average alignment error: ", meanvalue))
  dev.off()
}

FinalFiles <- function() {
  temp = list.files(pattern = "*.csv")
  filesList = lapply(temp, read.csv)

  if (length(filesList) == 0){
    return()
  }

  siteTable = data.frame()

  a = 1

  while (a < length(filesList) + 1) {
    file = filesList[[a]]

    meanErr = 0
    b = 2
    while (b < nrow(file) + 1) {
      #meanErr = meanErr + file[b, 7]
      meanErr = meanErr + file[b, 12]
      b = b + 1
    }
    meanErr = meanErr / (nrow(file) - 1)
    siteTable = rbind(siteTable, c(file[2, 2], file[2, 3], file[2, 4], format(round(meanErr, 2), nsmall = 2)))
    a = a + 1
  }
  name = gsub("_", " ", paste0(file[2, 4], "_overall_alignments"))


  starNames = unique(siteTable[, 1])
  dateNames = unique(siteTable[, 2])

  newTable = data.frame(matrix(ncol = length(dateNames), nrow = length(starNames)))

  newTable[is.na(newTable)] = "-"

  colnames(newTable) = dateNames
  newTable = newTable[, order(ncol(newTable):1)]
  rownames(newTable) = starNames

  c = 1

  while (c < nrow(siteTable) + 1) {
    newTable[siteTable[c, 1], siteTable[c, 2]] = siteTable[c, 4]

    c = c + 1
  }

  tempwd = getwd()

  setwd('..')

  dir.create(paste0(getwd(), "/", "final_files"), showWarnings = FALSE)

  setwd(paste0(getwd(), "/", "final_files"))

  pdf(file = (paste0(name, ".pdf")))

  cols = matrix("black", nrow(newTable), ncol(newTable))
  cols[newTable < 3] = "red"
  cols[newTable == "-"] = "black"
  tt = ttheme_default(
    core = list(
      fg_params = list(col = cols),
      bg_params = list(col = NA)
    ),
    rowhead = list(bg_params = list(col = NA)),
    colhead = list(bg_params = list(col = NA))
  )

  g = tableGrob(newTable, theme = tt)

  title = file[2, 4]

  grid.arrange(g, top = title)

  dev.off()

  newTable$Site = siteTable[1, 3]

  write.csv(file = (paste0(name, ".csv")), x = newTable)

  setwd(tempwd)
}

StarTable <- function() {
  startwd = getwd()

  setwd(paste0(startwd, "/final_files"))
  temp = list.files(pattern = "*.csv")
  filesList = lapply(temp, read.csv,check.names = FALSE)
  siteTable = data.frame()

  a = 1

  while (a < length(filesList) + 1) {
    file = filesList[[a]]
    siteTable = rbind(siteTable, file)

    a = a + 1
  }

  starNames = unique(siteTable[, 1])
  colnam = colnames(filesList[[1]])
  dir.create(paste0(getwd(), "/", "star_finals"), showWarnings = FALSE)

  setwd(paste0(getwd(), "/", "star_finals"))
  for (star in starNames) {
    name = gsub("_", " ", star, "_overall_alignments")
    siteNames = c()
    d = 1
    tempTable = siteTable

    tempTable = tempTable[tempTable[,1] == star,]

    colnames(tempTable) = colnam
    rownames(tempTable) = tempTable[, ncol(tempTable)]
    tempTable = tempTable[, 2:(ncol(tempTable) - 1)]

    pdf(file = (paste0(name, ".pdf")))

    cols = matrix("black", nrow(tempTable), ncol(tempTable))
    cols[tempTable < 3] = "red"
    cols[tempTable == "-"] = "black"
    tt = ttheme_default(
      core = list(
        fg_params = list(col = cols),
        bg_params = list(col = NA)
      ),
      rowhead = list(bg_params = list(col = NA)),
      colhead = list(bg_params = list(col = NA))
    )

    g = tableGrob(tempTable, theme = tt)

    title = star

    grid.arrange(g, top = title)

    dev.off()

    write.csv(file = (paste0(name, ".csv")), x = tempTable)
  }
  setwd(startwd)
}


setwd("C:/Users/robba/Documents/R/TarxienCore/TarxienCore")
yearRange = c(-3000, -2500, -2000, -1500, -1000)
TarxienCore(
  "test1.csv",
  cycle = TRUE,
  fastMode = TRUE,
  runstars = TRUE,
  runsun = FALSE,
  yearRange = yearRange
)
