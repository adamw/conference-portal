-- MySQL dump 10.13  Distrib 5.1.30, for apple-darwin9.4.0 (i386)
--
-- Host: localhost    Database: conference2
-- ------------------------------------------------------
-- Server version	5.1.30

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `conference`
--

DROP TABLE IF EXISTS `conference`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `conference` (
  `name` varchar(32) DEFAULT NULL,
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `desc_c` varchar(128) DEFAULT NULL,
  `state` int(11) DEFAULT NULL,
  `mainmenuitem` bigint(20) unsigned DEFAULT NULL,
  `dateend` date DEFAULT NULL,
  `datestart` date DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `conference_mainmenuitem` (`mainmenuitem`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `conference`
--

LOCK TABLES `conference` WRITE;
/*!40000 ALTER TABLE `conference` DISABLE KEYS */;
INSERT INTO `conference` VALUES ('Javarsovia 2010',1,'Największa darmowa konferencja Javowa w Polsce',1,1,'2010-06-26','2010-06-26');
/*!40000 ALTER TABLE `conference` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `configuration`
--

DROP TABLE IF EXISTS `configuration`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `configuration` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `active_conference` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `configuration_active_conference` (`active_conference`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `configuration`
--

LOCK TABLES `configuration` WRITE;
/*!40000 ALTER TABLE `configuration` DISABLE KEYS */;
INSERT INTO `configuration` VALUES (1,1);
/*!40000 ALTER TABLE `configuration` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `file_t`
--

DROP TABLE IF EXISTS `file_t`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `file_t` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `content` mediumblob,
  `mimetype` varchar(256) DEFAULT NULL,
  `uniqueid` varchar(16) DEFAULT NULL,
  `savetime` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `file_t_uniqueid` (`uniqueid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `file_t`
--

LOCK TABLES `file_t` WRITE;
/*!40000 ALTER TABLE `file_t` DISABLE KEYS */;
/*!40000 ALTER TABLE `file_t` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `menuitem`
--

DROP TABLE IF EXISTS `menuitem`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `menuitem` (
  `parent` bigint(20) unsigned DEFAULT NULL,
  `position` int(11) DEFAULT NULL,
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `title` varchar(1000) DEFAULT NULL,
  `linkcontent` varchar(1000) DEFAULT NULL,
  `pagecontent` varchar(10000) DEFAULT NULL,
  `pagepath` varchar(1000) DEFAULT NULL,
  `menu_item_type` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `menuitem_parent` (`parent`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `menuitem`
--

LOCK TABLES `menuitem` WRITE;
/*!40000 ALTER TABLE `menuitem` DISABLE KEYS */;
INSERT INTO `menuitem` VALUES (NULL,0,1,'','','','',0),(1,0,2,'Administracja','','','',5),(1,1,3,'Powrót do strony głównej','http://www.javarsovia.pl/','','',1),(1,2,4,'Użytkownik','','','',4),(1,3,5,'Call 4 Papers','/c4p/','','',1),(1,4,6,'Logout','','<div>\nZostałeś wylogowany.\n</div>','logout',6),(1,5,7,'Witamy!','','<div>\nTutaj możesz:\n<ul>\n<li>a</li>\n<li>b</li>\n</ul>\n</div>','index',6);
/*!40000 ALTER TABLE `menuitem` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `paper`
--

DROP TABLE IF EXISTS `paper`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `paper` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `shortdescription` varchar(3000) DEFAULT NULL,
  `user_c` bigint(20) unsigned DEFAULT NULL,
  `title` varchar(256) DEFAULT NULL,
  `conference` bigint(20) unsigned DEFAULT NULL,
  `status` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `paper_user_c` (`user_c`),
  KEY `paper_conference` (`conference`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `paper`
--

LOCK TABLES `paper` WRITE;
/*!40000 ALTER TABLE `paper` DISABLE KEYS */;
/*!40000 ALTER TABLE `paper` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `registration`
--

DROP TABLE IF EXISTS `registration`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `registration` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `user_c` bigint(20) unsigned DEFAULT NULL,
  `conference` bigint(20) unsigned DEFAULT NULL,
  `confirmationcode` varchar(10) DEFAULT NULL,
  `confirmed` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `registration_user_c` (`user_c`),
  KEY `registration_conference` (`conference`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `registration`
--

LOCK TABLES `registration` WRITE;
/*!40000 ALTER TABLE `registration` DISABLE KEYS */;
/*!40000 ALTER TABLE `registration` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `room`
--

DROP TABLE IF EXISTS `room`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `room` (
  `name` varchar(32) DEFAULT NULL,
  `position` int(11) DEFAULT NULL,
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `conference` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `room_conference` (`conference`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `room`
--

LOCK TABLES `room` WRITE;
/*!40000 ALTER TABLE `room` DISABLE KEYS */;
/*!40000 ALTER TABLE `room` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `slot`
--

DROP TABLE IF EXISTS `slot`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `slot` (
  `start_c` time DEFAULT NULL,
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `end_c` time DEFAULT NULL,
  `conference` bigint(20) unsigned DEFAULT NULL,
  `room` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `slot_conference` (`conference`),
  KEY `slot_room` (`room`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `slot`
--

LOCK TABLES `slot` WRITE;
/*!40000 ALTER TABLE `slot` DISABLE KEYS */;
/*!40000 ALTER TABLE `slot` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sourceoptions`
--

DROP TABLE IF EXISTS `sourceoptions`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `sourceoptions` (
  `value` varchar(128) DEFAULT NULL,
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `sourceoptions`
--

LOCK TABLES `sourceoptions` WRITE;
/*!40000 ALTER TABLE `sourceoptions` DISABLE KEYS */;
/*!40000 ALTER TABLE `sourceoptions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `userinterested`
--

DROP TABLE IF EXISTS `userinterested`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `userinterested` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `user_c` bigint(20) unsigned DEFAULT NULL,
  `paper` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `userinterested_user_c` (`user_c`),
  KEY `userinterested_paper` (`paper`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `userinterested`
--

LOCK TABLES `userinterested` WRITE;
/*!40000 ALTER TABLE `userinterested` DISABLE KEYS */;
/*!40000 ALTER TABLE `userinterested` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `users` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `firstname` varchar(32) DEFAULT NULL,
  `lastname` varchar(32) DEFAULT NULL,
  `email` varchar(48) DEFAULT NULL,
  `locale` varchar(16) DEFAULT NULL,
  `timezone` varchar(32) DEFAULT NULL,
  `password_pw` varchar(48) DEFAULT NULL,
  `password_slt` varchar(20) DEFAULT NULL,
  `bio` varchar(2048) DEFAULT NULL,
  `validated` tinyint(1) DEFAULT NULL,
  `uniqueid` varchar(32) DEFAULT NULL,
  `face` bigint(20) unsigned DEFAULT NULL,
  `sex` int(11) DEFAULT NULL,
  `hometown` longtext,
  `superuser` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `users_email` (`email`),
  KEY `users_uniqueid` (`uniqueid`),
  KEY `users_face` (`face`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `users`
--

LOCK TABLES `users` WRITE;
/*!40000 ALTER TABLE `users` DISABLE KEYS */;
INSERT INTO `users` VALUES (1,'admin','admin','admin@admin.com','pl','Europe/Warsaw','VbzcERo5tFmuGQuLdakKHK6cyGA=','YUEXYOHHWLHC514Q','',1,'2JQYEECZGWLZJH33OAXXJOUSGQIRKV3O',NULL,1,'warszawa',1);
/*!40000 ALTER TABLE `users` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2010-04-06  6:37:52
